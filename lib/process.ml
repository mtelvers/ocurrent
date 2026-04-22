let () =
  Random.self_init ()

let pp_cmd : Format.formatter -> string list -> unit =
  let sep = Fmt.(const string) " " in
  Fmt.(list ~sep (quote string))

let check_status pp_cmd cmd = function
  | `Exited 0 -> Ok ()
  | `Exited 127 ->
    let cmd_name =
      match cmd with
      | [] -> None
      | x :: _ -> Some x
    in
    (match cmd_name with
     | Some name -> Fmt.error_msg "%t exited with status %d. Is %s installed?" pp_cmd 127 name
     | None -> Fmt.error_msg "%t exited with status %d" pp_cmd 127)
  | `Exited x -> Fmt.error_msg "%t exited with status %d" pp_cmd x
  | `Signaled x -> Fmt.error_msg "%t failed with signal %a" pp_cmd Fmt.Dump.signal x

let make_tmp_dir ?(prefix = "tmp-") ?(mode = 0o700) parent =
  let rec mktmp = function
    | 0 -> Fmt.failwith "Failed to generate temporary directory name!"
    | n ->
      let tmppath =
        Printf.sprintf "%s/%s%x" parent prefix (Random.int 0x3fffffff)
      in
      try
        Unix.mkdir tmppath mode;
        tmppath
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        Log.warn (fun f -> f "Temporary directory %s already exists!" tmppath);
        mktmp (n - 1)
  in
  mktmp 10

let rec rm_f_tree path =
  let info = Unix.lstat path in
  match info.Unix.st_kind with
  | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
  | Unix.S_FIFO ->
    (try Unix.unlink path
     with Unix.Unix_error (Unix.EACCES, _, _) when Sys.win32 ->
       (* Try removing the read-only attribute before retrying unlink. *)
       (try
          let { Unix.st_perm; _ } = Unix.lstat path in
          Unix.chmod path 0o666;
          (try Unix.unlink path
           with _ ->
             (* If removal still failed, restore original permissions *)
             (try Unix.chmod path st_perm with _ -> ());
             raise Exit)
        with _ -> raise Exit))
  | Unix.S_DIR ->
    Unix.chmod path 0o700;
    let entries = Sys.readdir path in
    Array.iter (function
      | "." | ".." -> ()
      | leaf -> rm_f_tree (Filename.concat path leaf)) entries;
    Unix.rmdir path

let with_tmpdir ?prefix fn =
  let tmpdir = make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  Fun.protect
    (fun () -> fn (Fpath.v tmpdir))
    ~finally:(fun () ->
      try rm_f_tree tmpdir
      with ex -> Log.warn (fun f -> f "Error cleaning up %s: %a" tmpdir Fmt.exn ex))

let pp_command pp_cmd cmd f = Fmt.pf f "Command %a" pp_cmd cmd

let copy_to_log ~job src =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read src buf with
    | n -> Job.write job (Cstruct.to_string ~len:n buf); loop ()
    | exception End_of_file -> ()
  in
  loop ()

let add_shutdown_hooks ~cancellable ~job ~cmd proc =
  let terminate () =
    try Eio.Process.signal proc Sys.sigterm
    with _ -> ()
  in
  if cancellable then
    Job.on_cancel job (fun reason ->
      Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
      terminate ())
  else
    (* Always terminate process if the job ends: *)
    Switch.add_hook_or_exec job.Job.switch terminate

let exec ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ?env ~cancellable ~job cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  Eio.Switch.run @@ fun sw ->
  let mgr = Engine_env.process_mgr () in
  let cwd =
    Option.map (fun p ->
      Eio.Path.(Engine_env.fs () / Fpath.to_string p)) cwd
  in
  let stdin_r, stdin_w = Eio.Process.pipe ~sw mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let proc =
    Eio.Process.spawn ~sw mgr
      ?cwd
      ?env
      ~stdin:stdin_r
      ~stdout:stdout_w
      ~stderr:stdout_w
      cmd
  in
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  add_shutdown_hooks ~cancellable ~job ~cmd proc;
  let stdin_result = ref (Ok ()) in
  Eio.Fiber.both
    (fun () ->
      (try
         Eio.Flow.copy_string stdin stdin_w;
         Eio.Flow.close stdin_w
       with ex ->
         stdin_result := Error (`Msg (Printexc.to_string ex));
         (try Eio.Flow.close stdin_w with _ -> ())))
    (fun () -> copy_to_log ~job stdout_r);
  let status = Eio.Process.await proc in
  match check_status pp_error_command cmd status with
  | Ok () -> !stdin_result
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  Eio.Switch.run @@ fun sw ->
  let mgr = Engine_env.process_mgr () in
  let cwd =
    Option.map (fun p ->
      Eio.Path.(Engine_env.fs () / Fpath.to_string p)) cwd
  in
  let stdin_r, stdin_w = Eio.Process.pipe ~sw mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc =
    Eio.Process.spawn ~sw mgr
      ?cwd
      ~stdin:stdin_r
      ~stdout:stdout_w
      ~stderr:stderr_w
      cmd
  in
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  Eio.Flow.close stderr_w;
  add_shutdown_hooks ~cancellable ~job ~cmd proc;
  let stdin_result = ref (Ok ()) in
  let stdout = ref "" in
  Eio.Fiber.all [
    (fun () ->
      (try
         Eio.Flow.copy_string stdin stdin_w;
         Eio.Flow.close stdin_w
       with ex ->
         stdin_result := Error (`Msg (Printexc.to_string ex));
         (try Eio.Flow.close stdin_w with _ -> ())));
    (fun () ->
      stdout := Eio.Buf_read.(of_flow ~max_size:max_int stdout_r |> take_all));
    (fun () -> copy_to_log ~job stderr_r);
  ];
  let status = Eio.Process.await proc in
  match check_status pp_error_command cmd status with
  | Error _ as e -> e
  | Ok () ->
    match !stdin_result with
    | Error _ as e -> e
    | Ok () -> Ok !stdout
