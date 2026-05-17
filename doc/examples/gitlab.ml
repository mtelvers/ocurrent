(* Usage: gitlab.exe GITLAB_USER/REPO/PROJECT_ID --gitlab-token-file GITLAB-TOKEN-FILE \
            --gitlab-webhook-secret-file GITLAB-WEBHOOK-SECRET

   This pipeline monitors a GitLab repository and uses Docker to build the
   the latest version on all branches and Merge Requests. Updates to the GitLab
   repository are delivered as webhooks to `/webhooks/gitlab`, some suitable configuration
   and forwarding of these events is required. eg smee.io
*)

let program_name = "gitlab"

open Current.Syntax

module Git = Current_git
module Gitlab = Current_gitlab

(* Limit to one build at a time. *)
let pool = Current.Pool.create ~label:"docker" 1

let () = Prometheus_unix.Logging.init ()

(* Link for GitLab statuses. *)
let url = Uri.of_string "http://localhost:8080"

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base_hash =
  let open Dockerfile in
  from base_hash @@
  run "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam" @@
  run "opam init --reinit -n" @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  run "opam install . --show-actions --deps-only -t" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."
  |> string_of_t

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let gitlab_status_of_state = function
  | Ok _              -> Gitlab.Api.Status.v ~url `Success ~description:"Passed" ~name:program_name
  | Error (`Active _) -> Gitlab.Api.Status.v ~url `Pending ~name:program_name
  | Error (`Msg m)    -> Gitlab.Api.Status.v ~url `Failure ~description:m ~name:program_name

let main config mode gitlab_config repo =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let has_role = Current_web.Site.allow_all in
  let gitlab_p, gitlab_r = Eio.Promise.create () in
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let caps = Current_cache.caps_of_engine engine in
      let git = Current_git.create ~caps in
      let module Docker = (val Current_docker.default ~caps ~git) in
      let gitlab = Gitlab.Api.create ~caps ~net gitlab_config in
      Eio.Promise.resolve gitlab_r gitlab;
      let dockerfile =
        let+ base = Docker.pull ~schedule:weekly "ocaml/opam:alpine-3.13-ocaml-4.13" in
        `Contents (dockerfile ~base_hash:(Docker.Image.hash base))
      in
      Gitlab.Api.ci_refs gitlab ~staleness:(Duration.of_day 90) repo
      |> Current.list_iter (module Gitlab.Api.Commit) @@ fun head ->
      let src = Git.fetch git (Current.map Gitlab.Api.Commit.id head) in
      Docker.build ~pool ~pull:false ~dockerfile (`Git src)
      |> Current.state
      |> Current.map gitlab_status_of_state
      |> Gitlab.Api.Commit.set_status head program_name)
  in
  let gitlab = Eio.Promise.await gitlab_p in
  let routes =
    Routes.(s "webhooks" / s "gitlab" /? nil @--> Gitlab.webhook ~webhook_secret:(Gitlab.Api.webhook_secret gitlab)) ::
    Current_web.routes engine
  in
  let site = Current_web.Site.(v ~has_role) ~name:program_name routes in
  Current_web.run ~net ~mode site ()

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Monitor a GitLab repository." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_gitlab.Api.cmdliner $ Current_gitlab.Repo_id.cmdliner)

let () = exit @@ Cmd.eval cmd
