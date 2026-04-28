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

let make_tmp_dir ~fs ?(prefix = "tmp-") ?(mode = 0o700) parent =
  let rec mktmp = function
    | 0 -> Fmt.failwith "Failed to generate temporary directory name!"
    | n ->
      let path_str = Printf.sprintf "%s/%s%x" parent prefix (Random.int 0x3fffffff) in
      let eio_path = Eio.Path.(fs / path_str) in
      try
        Eio.Path.mkdir ~perm:mode eio_path;
        eio_path, Fpath.v path_str
      with Eio.Io (Eio.Fs.E (Already_exists _), _) ->
        Log.warn (fun f -> f "Temporary directory %s already exists!" path_str);
        mktmp (n - 1)
  in
  mktmp 10

let with_tmpdir ~job ?prefix fn =
  let fs = Job.fs job in
  let eio_path, fpath = make_tmp_dir ~fs ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  Fun.protect
    (fun () -> fn fpath)
    ~finally:(fun () ->
      try Eio.Path.rmtree ~missing_ok:true eio_path
      with ex -> Log.warn (fun f -> f "Error cleaning up %a: %a" Fpath.pp fpath Fmt.exn ex))

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
  (* For cancellable=false we don't need any hook: the process is spawned
     on [exec]'s own [Eio.Switch.run] scope, so Eio cancellation propagates
     naturally if the caller is cancelled. *)
  if cancellable then
    Job.on_cancel job (fun reason ->
      Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
      try Eio.Process.signal proc Sys.sigterm
      with _ -> ())

let exec ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ?env ~cancellable ~job cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  Eio.Switch.run @@ fun sw ->
  let mgr = Job.process_mgr job in
  let cwd =
    Option.map (fun p ->
      Eio.Path.(Job.fs job / Fpath.to_string p)) cwd
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
  let stdin_done, set_stdin_done = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () ->
      let result =
        try
          Eio.Flow.copy_string stdin stdin_w;
          Eio.Flow.close stdin_w;
          Ok ()
        with ex ->
          (try Eio.Flow.close stdin_w with _ -> ());
          Error (`Msg (Printexc.to_string ex))
      in
      Eio.Promise.resolve set_stdin_done result)
    (fun () -> copy_to_log ~job stdout_r);
  let status = Eio.Process.await proc in
  match check_status pp_error_command cmd status with
  | Ok () -> Eio.Promise.await stdin_done
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  Eio.Switch.run @@ fun sw ->
  let mgr = Job.process_mgr job in
  let cwd =
    Option.map (fun p ->
      Eio.Path.(Job.fs job / Fpath.to_string p)) cwd
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
  let stdin_done, set_stdin_done = Eio.Promise.create () in
  let stdout_done, set_stdout_done = Eio.Promise.create () in
  Eio.Fiber.all [
    (fun () ->
      let result =
        try
          Eio.Flow.copy_string stdin stdin_w;
          Eio.Flow.close stdin_w;
          Ok ()
        with ex ->
          (try Eio.Flow.close stdin_w with _ -> ());
          Error (`Msg (Printexc.to_string ex))
      in
      Eio.Promise.resolve set_stdin_done result);
    (fun () ->
      (* 100 MiB is enough for all realistic check_output use; prevents
         unbounded memory growth if a subprocess goes mad. *)
      let s = Eio.Buf_read.(of_flow ~max_size:(100 * 1024 * 1024) stdout_r |> take_all) in
      Eio.Promise.resolve set_stdout_done s);
    (fun () -> copy_to_log ~job stderr_r);
  ];
  let status = Eio.Process.await proc in
  match check_status pp_error_command cmd status with
  | Error _ as e -> e
  | Ok () ->
    match Eio.Promise.await stdin_done with
    | Error _ as e -> e
    | Ok () -> Ok (Eio.Promise.await stdout_done)
