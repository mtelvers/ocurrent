(* Usage: github.exe SOURCE --github-token-file GITHUB-TOKEN-FILE --github-webhook-secret-file GITHUB-APP-SECRET

   Given a Github repository SOURCE, build the latest version on the default branch
   using Docker and OCaml 4.13. Updates to the GitHub repository will trigger webhooks on
   "webhooks/github", so some suitable forwarding of webhooks from GitHub to localhost needs
   to be setup eg smee.io, along with a suitable token and webhook secret.

*)

let program_name = "github"

open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let () = Prometheus_unix.Logging.init ()

(* Link for GitHub statuses. *)
let url = Uri.of_string "http://localhost:8080"

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  run "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam" @@
  run "opam init --reinit -n" @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  env ["OPAMERRLOGLEN", "0"] @@
  run "opam install . --show-actions --deps-only -t" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."
  |> string_of_t

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let github_status_of_state = function
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let pipeline ~docker ~git ~github ~repo () =
  let head = Github.Api.head_commit github repo in
  let src = Git.fetch git (Current.map Github.Api.Commit.id head) in
  let dockerfile =
    let+ base = Docker.pull docker ~schedule:weekly "ocaml/opam:alpine-3.13-ocaml-4.13" in
    `Contents (dockerfile ~base)
  in
  Docker.build docker ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocurrent"

let main config mode github_config repo =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let has_role = Current_web.Site.allow_all in
  let github_p, github_r = Eio.Promise.create () in
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let git = Current_git.create ~engine in
      let docker = Docker.create ~engine ~git in
      let github = Github.Api.create ~engine ~net github_config in
      Eio.Promise.resolve github_r github;
      pipeline ~docker ~git ~github ~repo ())
  in
  let github = Eio.Promise.await github_p in
  (* this example does not have support for looking up job_ids for a commit *)
  let get_job_ids = (fun ~owner:_owner ~name:_name ~hash:_hash -> []) in
  let routes =
    Routes.(s "webhooks" / s "github" /? nil @--> Github.webhook ~engine ~get_job_ids ~webhook_secret:(Github.Api.webhook_secret github)) ::
    Current_web.routes engine
  in
  let site = Current_web.Site.(v ~has_role) ~name:program_name routes in
  Current_web.run ~sw ~net ~mode site

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.pos 0 (Arg.some Github.Repo_id.cmdliner) None @@
  Arg.info
    ~doc:"The GitHub repository (owner/name) to monitor."
    ~docv:"REPO"
    []

let cmd =
  let doc = "Monitor a GitHub repository." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.Api.cmdliner $ repo))

let () = exit @@ Cmd.eval cmd
