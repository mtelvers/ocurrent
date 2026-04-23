open Current.Syntax

module Cmd = struct
  let exec_or_fail ?cwd ~name cmd =
    let cwd =
      Option.map (fun p ->
        Eio.Path.(Current.Engine_env.fs () / p)) cwd
    in
    let mgr = Current.Engine_env.process_mgr () in
    let cmd_s = String.concat " " cmd in
    Eio.Switch.run @@ fun sw ->
    let proc = Eio.Process.spawn ~sw mgr ?cwd cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> ()
    | `Exited n ->
      Alcotest.(check int) (Printf.sprintf "Process %s: %s" name cmd_s) 0 n
    | `Signaled _ -> Alcotest.fail "Process received signal."

  let mkdir ?cwd dir =
    let cmd = [ "mkdir"; dir ] in
    exec_or_fail ?cwd ~name:"mkdir" cmd

  let rm ?cwd cmd =
    let cmd = "rm" :: cmd in
    exec_or_fail ?cwd ~name:"rm" cmd

  let mv ?cwd origin target =
    let cmd = [ "mv"; origin; target ] in
    exec_or_fail ?cwd ~name:"mv" cmd

  let touch ?cwd file =
    let cmd = [ "touch"; file ] in
    exec_or_fail ?cwd ~name:"touch" cmd

  let echo_to ?(cwd = "./") file content =
    let file = Filename.concat cwd file in
    let ch = open_out file in
    Fun.protect ~finally:(fun () -> close_out ch) (fun () ->
      output_string ch content;
      output_char ch '\n')

  let git ?cwd cmd =
    let cmd = "git" :: "-c" :: "protocol.file.allow=always" :: cmd in
    exec_or_fail ?cwd ~name:"git" cmd

  let git_with ?cwd ~path cmd =
    let cmd = "-C" :: path :: cmd in
    git ?cwd cmd
end

let results = Eio.Stream.create max_int
let push_result x = Eio.Stream.add results x

module Show_files = struct
  type t = unit

  let id = "show-files"

  module Key = struct
    include Current_git.Commit

    let digest t = Current_git.Commit_id.digest (id t)
  end

  module Value = Current.Unit

  let build () job commit =
    Current.Job.start job ~level:Current.Level.Harmless;
    Current_git.with_checkout ~job commit (fun tmpdir ->
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

let show_files commit =
  Current.component "show_files"
  |> let> commit = commit in
     SF.get () commit

let init root =
  let cwd = Fpath.to_string root in
  let dir = "sub" in
  Cmd.mkdir ~cwd dir;
  Cmd.git ~cwd [ "init"; "-q"; dir ];
  let file = "sub/file" in
  Cmd.echo_to ~cwd file "sub";
  Cmd.git_with ~cwd ~path:"sub" [ "config"; "user.name"; "Name" ];
  Cmd.git_with ~cwd ~path:"sub" [ "config"; "user.email"; "test@example.com" ];
  Cmd.git_with ~cwd ~path:"sub" [ "add"; "file" ];
  Cmd.git_with ~cwd ~path:"sub"
    [ "commit"; "-q"; "-a"; "-m"; "Initial submodule commit" ];
  let dir = "main" in
  Cmd.mkdir ~cwd dir;
  Cmd.git ~cwd [ "init"; "-q"; dir ];
  let file = "main/file" in
  Cmd.echo_to ~cwd file "main";
  Cmd.git_with ~cwd ~path:"main" [ "add"; "file" ];
  Cmd.git_with ~cwd ~path:"main" [ "submodule"; "add"; "-q"; "../sub" ];
  Cmd.git_with ~cwd ~path:"main" [ "config"; "user.name"; "Name" ];
  Cmd.git_with ~cwd ~path:"main" [ "config"; "user.email"; "test@example.com" ];
  Cmd.git_with ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Initial main commit" ]

let remove root =
  let cwd = Fpath.to_string root in
  Cmd.rm ~cwd [ "main/.gitmodules" ];
  Cmd.rm ~cwd [ "-r"; "main/sub" ];
  Cmd.git_with ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Remove submodule" ]

let add_back cwd =
  let cwd = Fpath.to_string cwd in
  Cmd.git_with ~cwd ~path:"main"
    [ "submodule"; "add"; "--force"; "-q"; "../sub" ];
  Cmd.git_with ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Restore submodule" ]

let update_submodules cwd =
  let cwd = Fpath.to_string cwd in
  Cmd.mv ~cwd "sub" "newsub";
  Cmd.echo_to ~cwd "newsub/file2" "sub2";
  Cmd.git_with ~cwd ~path:"newsub" [ "add"; "file2" ];
  Cmd.git_with ~cwd ~path:"newsub" [ "config"; "user.name"; "Name" ];
  Cmd.git_with ~cwd ~path:"newsub"
    [ "config"; "user.email"; "test@example.com" ];
  Cmd.git_with ~cwd ~path:"newsub" [ "commit"; "-q"; "-a"; "-m"; "sub2" ]

let move_submodule cwd_f =
  let cwd = Fpath.to_string cwd_f in
  Cmd.git_with ~cwd ~path:"main" [ "submodule"; "deinit"; "-q"; "--all" ];
  Cmd.rm ~cwd [ "main/.gitmodules" ];
  Cmd.touch ~cwd "main/.gitmodules";
  Cmd.rm ~cwd [ "-r"; "main/sub" ];
  let path = Fpath.(add_seg cwd_f "newsub" |> to_string) in
  Cmd.git_with ~cwd ~path:"main"
    [ "submodule"; "add"; "--force"; "-q"; path; "sub" ];
  Cmd.git_with ~cwd ~path:"main" [ "submodule"; "sync"; "-q" ];
  Cmd.git_with ~cwd ~path:"main/sub" [ "pull"; "-q"; "origin" ];
  Cmd.git_with ~cwd ~path:"main" [ "commit"; "-q"; "-a"; "-m"; "Move module" ]

let check_result label expected =
  let value = Eio.Stream.take results in
  Alcotest.(check (option (list string)) label value expected)

let test () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Current.Engine_env.init ~sw ~env;
  let dir =
    let base = Filename.get_temp_dir_name () in
    let name = Printf.sprintf "current-git-%d" (Random.bits ()) in
    let path = Filename.concat base name in
    Unix.mkdir path 0o755;
    Fpath.v path
  in
  init dir;
  let repo = Current_git.Local.v (Fpath.add_seg dir "main") in
  let pipeline () =
    let remote_commit = Current_git.Local.head_commit repo in
    let id = Current.map Current_git.Commit.id remote_commit in
    let clone = Current_git.fetch id in
    let+ result = Current.catch (show_files clone) in
    match result with
    | Ok () -> ()
    | Error (`Msg m) -> push_result (Some [ m ])
  in
  let _engine = Current.Engine.create pipeline in
  let expected = Some [ "file"; "sub" ] in
  check_result "Initial state" expected;
  remove dir;
  let expected = Some [ "file" ] in
  check_result "After remove" expected;
  add_back dir;
  let expected = Some [ "file"; "sub" ] in
  check_result "After restore" expected;
  update_submodules dir;
  move_submodule dir;
  let expected = Some [ "file"; "sub" ] in
  check_result "Final state" expected

let () =
  Alcotest.run "current-git"
    [ ("mdx-like", [ Alcotest.test_case "full test" `Quick test ]) ]
