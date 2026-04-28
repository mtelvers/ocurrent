let program_name = "docker_build_local"

module Git = Current_git

let pull = false    (* Whether to check for updates using "docker build --pull" *)

let timeout = Duration.of_min 50    (* Max build time *)

let () = Prometheus_unix.Logging.init ()

(* included in doc/example_pipelines.md as code snippet *)
[@@@part "pipeline"]
(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline (module Docker : Current_docker.S.DOCKER) ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~pull ~timeout (`Git src) in
  Docker.run image ~args:["dune"; "exec"; "--"; "docker_build_local"; "--help"]

[@@@part "end-pipeline"]

let find_git_root ~process_mgr dir =
  let out =
    Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
      ["git"; "-C"; dir; "rev-parse"; "--show-toplevel"]
  in
  String.trim out

let main config mode repo =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let repo = find_git_root ~process_mgr repo in
  let repo = Git.Local.v ~sw ~process_mgr (Fpath.v repo) in
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let git = Current_git.create ~engine in
      let module Docker = Current_docker.Default () (struct
        let caps = Current_cache.caps_of_engine engine
        let git = git
      end) in
      pipeline (module Docker) ~repo ())
  in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Current_web.run ~net ~mode site

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.value @@
  Arg.pos 0 Arg.dir (Sys.getcwd ()) @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo))

let () = exit @@ Cmd.eval cmd
