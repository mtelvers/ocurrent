(* Usage: docker_service.exe --service NAME SOURCE

   Given a local Git repository SOURCE containing a Dockerfile, build the
   docker image whenever the Git repository changes, and at least once a week
   (in case the base image has changed). If this results in a new image, update
   service NAME (which must already exist; use "docker service create" first).

   e.g.

   $ docker service create --name my-service -p 9000:80 nginx
   $ dune exec -- ./doc/examples/docker_service.exe --service=my-service /src/service
*)

let program_name = "docker_service"

module Git = Current_git
module Docker = Current_docker.Default

let () = Prometheus_unix.Logging.init ()

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Run "docker build" on the latest commit in Git repository [repo]
   at least once a week, and redeploy [service] on changes. *)
let pipeline ~docker ~repo ~service () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build docker ~schedule:weekly ~pull:true (`Git src) in
  Docker.service docker ~name:service ~image ()

let main config mode service repo =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let repo = Git.Local.v ~sw ~process_mgr (Fpath.v repo) in
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let git = Current_git.create ~engine in
      let docker = Docker.create ~engine ~git in
      pipeline ~docker ~repo ~service ())
  in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Current_web.run ~net ~mode site

(* Command-line parsing *)

open Cmdliner

let service =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"The name of the Docker service to update."
    ~docv:"NAME"
    ["service"]

let repo =
  Arg.value @@
  Arg.pos 0 Arg.dir (Sys.getcwd ()) @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Keep a Docker SwarmKit service up-to-date." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ service $ repo))

let () = exit @@ Cmd.eval cmd
