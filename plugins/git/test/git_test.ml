open Current.Syntax

module Cmd = struct
  let exec_or_fail ~env ?cwd ~name cmd =
    let cwd =
      Option.map (fun p ->
        Eio.Path.(Eio.Stdenv.fs env / p)) cwd
    in
    let mgr = Eio.Stdenv.process_mgr env in
    let cmd_s = String.concat " " cmd in
    Eio.Switch.run @@ fun sw ->
    let proc = Eio.Process.spawn ~sw mgr ?cwd cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> ()
    | `Exited n ->
      Alcotest.(check int) (Printf.sprintf "Process %s: %s" name cmd_s) 0 n
    | `Signaled _ -> Alcotest.fail "Process received signal."

  let mkdir ~env ?cwd dir =
    let cmd = [ "mkdir"; dir ] in
    exec_or_fail ~env ?cwd ~name:"mkdir" cmd

  let rm ~env ?cwd cmd =
    let cmd = "rm" :: cmd in
    exec_or_fail ~env ?cwd ~name:"rm" cmd

  let mv ~env ?cwd origin target =
    let cmd = [ "mv"; origin; target ] in
    exec_or_fail ~env ?cwd ~name:"mv" cmd

  let touch ~env ?cwd file =
    let cmd = [ "touch"; file ] in
    exec_or_fail ~env ?cwd ~name:"touch" cmd

  let echo_to ?(cwd = "./") file content =
    let file = Filename.concat cwd file in
    let ch = open_out file in
    Fun.protect ~finally:(fun () -> close_out ch) (fun () ->
      output_string ch content;
      output_char ch '\n')

  let git ~env ?cwd cmd =
    let cmd = "git" :: "-c" :: "protocol.file.allow=always" :: cmd in
    exec_or_fail ~env ?cwd ~name:"git" cmd

  let git_with ~env ?cwd ~path cmd =
    let cmd = "-C" :: path :: cmd in
    git ~env ?cwd cmd
end

let results = Eio.Stream.create max_int
let push_result x = Eio.Stream.add results x

module Show_files = struct
  type t = Current_git.t

  let id = "show-files"

  module Key = struct
    include Current_git.Commit

    let digest t = Current_git.Commit_id.digest (id t)
  end

  module Value = Current.Unit

  let build git job commit =
    Current.Job.start job ~level:Current.Level.Harmless;
    Current_git.with_checkout git ~job commit (fun tmpdir ->
        let files =
          Sys.readdir (Fpath.to_string tmpdir)
          |> Array.to_list
          |> List.filter (fun x -> x.[0] <> '.')
          |> List.sort String.compare
        in
        push_result (Some files);
        Ok ())

  let pp = Current_git.Commit.pp
  let auto_cancel = false
end

module SF = Current_cache.Make (Show_files)

let show_files sf git commit =
  Current.component "show_files"
  |> let> commit = commit in
     SF.get sf git commit

let init ~env root =
  let cwd = Fpath.to_string root in
  let dir = "sub" in
  Cmd.mkdir ~env ~cwd dir;
  Cmd.git ~env ~cwd [ "init"; "-q"; dir ];
  let file = "sub/file" in
  Cmd.echo_to ~cwd file "sub";
  Cmd.git_with ~env ~cwd ~path:"sub" [ "config"; "user.name"; "Name" ];
  Cmd.git_with ~env ~cwd ~path:"sub" [ "config"; "user.email"; "test@example.com" ];
  Cmd.git_with ~env ~cwd ~path:"sub" [ "add"; "file" ];
  Cmd.git_with ~env ~cwd ~path:"sub"
    [ "commit"; "-q"; "-a"; "-m"; "Initial submodule commit" ];
  let dir = "main" in
  Cmd.mkdir ~env ~cwd dir;
  Cmd.git ~env ~cwd [ "init"; "-q"; dir ];
  let file = "main/file" in
  Cmd.echo_to ~cwd file "main";
  Cmd.git_with ~env ~cwd ~path:"main" [ "add"; "file" ];
  Cmd.git_with ~env ~cwd ~path:"main" [ "submodule"; "add"; "-q"; "../sub" ];
  Cmd.git_with ~env ~cwd ~path:"main" [ "config"; "user.name"; "Name" ];
  Cmd.git_with ~env ~cwd ~path:"main" [ "config"; "user.email"; "test@example.com" ];
  Cmd.git_with ~env ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Initial main commit" ]

let remove ~env root =
  let cwd = Fpath.to_string root in
  Cmd.rm ~env ~cwd [ "main/.gitmodules" ];
  Cmd.rm ~env ~cwd [ "-r"; "main/sub" ];
  Cmd.git_with ~env ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Remove submodule" ]

let add_back ~env cwd =
  let cwd = Fpath.to_string cwd in
  Cmd.git_with ~env ~cwd ~path:"main"
    [ "submodule"; "add"; "--force"; "-q"; "../sub" ];
  Cmd.git_with ~env ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Restore submodule" ]

let update_submodules ~env cwd =
  let cwd = Fpath.to_string cwd in
  Cmd.mv ~env ~cwd "sub" "newsub";
  Cmd.echo_to ~cwd "newsub/file2" "sub2";
  Cmd.git_with ~env ~cwd ~path:"newsub" [ "add"; "file2" ];
  Cmd.git_with ~env ~cwd ~path:"newsub" [ "config"; "user.name"; "Name" ];
  Cmd.git_with ~env ~cwd ~path:"newsub"
    [ "config"; "user.email"; "test@example.com" ];
  Cmd.git_with ~env ~cwd ~path:"newsub" [ "commit"; "-q"; "-a"; "-m"; "sub2" ]

let move_submodule ~env cwd_f =
  let cwd = Fpath.to_string cwd_f in
  Cmd.git_with ~env ~cwd ~path:"main" [ "submodule"; "deinit"; "-q"; "--all" ];
  Cmd.rm ~env ~cwd [ "main/.gitmodules" ];
  Cmd.touch ~env ~cwd "main/.gitmodules";
  Cmd.rm ~env ~cwd [ "-r"; "main/sub" ];
  let path = Fpath.(add_seg cwd_f "newsub" |> to_string) in
  Cmd.git_with ~env ~cwd ~path:"main"
    [ "submodule"; "add"; "--force"; "-q"; path; "sub" ];
  Cmd.git_with ~env ~cwd ~path:"main" [ "submodule"; "sync"; "-q" ];
  Cmd.git_with ~env ~cwd ~path:"main/sub" [ "pull"; "-q"; "origin" ];
  Cmd.git_with ~env ~cwd ~path:"main" [ "commit"; "-q"; "-a"; "-m"; "Move module" ]

let check_result label expected =
  let value = Eio.Stream.take results in
  Alcotest.(check (option (list string)) label value expected)

let test () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir =
    let base = Filename.get_temp_dir_name () in
    let name = Printf.sprintf "current-git-%d" (Random.bits ()) in
    let path = Filename.concat base name in
    Unix.mkdir path 0o755;
    Fpath.v path
  in
  init ~env dir;
  let repo =
    Current_git.Local.v
      ~sw ~process_mgr:(Eio.Stdenv.process_mgr env)
      (Fpath.add_seg dir "main")
  in
  let pipeline engine =
    let caps = Current_cache.caps_of_engine engine in
    let git = Current_git.create ~caps in
    let sf = SF.create ~caps in
    fun () ->
    let remote_commit = Current_git.Local.head_commit repo in
    let id = Current.map Current_git.Commit.id remote_commit in
    let clone = Current_git.fetch git id in
    let+ result = Current.catch (show_files sf git clone) in
    match result with
    | Ok () -> ()
    | Error (`Msg m) -> push_result (Some [ m ])
  in
  let _engine = Current.Engine.create ~sw ~env (fun engine -> pipeline engine ()) in
  let expected = Some [ "file"; "sub" ] in
  check_result "Initial state" expected;
  remove ~env dir;
  let expected = Some [ "file" ] in
  check_result "After remove" expected;
  add_back ~env dir;
  let expected = Some [ "file"; "sub" ] in
  check_result "After restore" expected;
  update_submodules ~env dir;
  move_submodule ~env dir;
  let expected = Some [ "file"; "sub" ] in
  check_result "Final state" expected

let () =
  Alcotest.run "current-git"
    [ ("mdx-like", [ Alcotest.test_case "full test" `Quick test ]) ]
