(* Usage: docker_custom.exe

   This fetches the latest nginx image (once a week) and tests it. The test
   starts the container running nginx and then tries execing curl inside it
   too. This demonstrates how to write custom pipeline stages to cover cases
   that the Docker plugin doesn't.
*)

let program_name = "docker_custom"

open Current.Syntax

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let () = Prometheus_unix.Logging.init ()

(* The test command to run. You might want to make this part of the key if it
   should be configurable. *)
let test_command = ["curl"; "-Ss"; "--fail"; "http://localhost/"]

module Make_test (D : Current_docker.S.DOCKER) = struct
  module Raw = D.Raw

  let id = "docker-custom-test"         (* A unique ID for the results database *)

  type t = No_context

  module Key = struct
    type t = {
      image : D.Image.t;
    }

    let digest { image } =
      Yojson.Safe.to_string @@ `Assoc [
        "image", `String (D.Image.hash image);
      ]

    let pp f t =
        Fmt.pf f "Test %a" D.Image.pp t.image
  end

  module Value = Current.Unit

  let run image = Raw.Cmd.docker ["container"; "run"; "-d"; D.Image.hash image]
  let exec id args = Raw.Cmd.docker ("container" :: "exec" :: "-i" :: id :: args)

  let build No_context job { Key.image } =
    Current.Job.start job ~level:Current.Level.Mostly_harmless;
    (* Start the container running: *)
    Raw.Cmd.with_container ~job ~kill_on_cancel:true (run image) @@ fun id ->
    Current.Job.log job "Waiting 1 second to let HTTP server start...";
    Eio.Time.sleep (Current.Job.clock job) 1.0;
    (* Test the container's service: *)
    Current.Process.exec ~cancellable:true ~job (exec id test_command)

  let auto_cancel = true

  let pp = Key.pp
end

let main config mode =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let caps = Current_cache.caps_of_engine engine in
      let git = Current_git.create ~caps in
      let module Docker = Current_docker.Default () (struct
        let caps = caps
        let git = git
      end) in
      let module Test = Make_test (Docker) in
      let module Test_cache = Current_cache.Make (Test) in
      let test_cache = Test_cache.create ~caps in
      (* Test a Docker image by running it and then execing curl inside it. *)
      let test image =
        Current.component "test with@,@[<h>%a@]" Fmt.(list ~sep:sp string) test_command |>
        let> image = image in
        Test_cache.get test_cache Test.No_context { Test.Key.image }
      in
      (* Build a docker image with nginx and curl and then test it. *)
      let dockerfile =
        let+ base = Docker.pull ~schedule:weekly "nginx" in
        `Contents Dockerfile.(
            from (Docker.Image.hash base) @@
            run "apt-get update && apt-get install -y curl --no-install-recommends"
            |> string_of_t
          )
      in
      test (Docker.build ~pull:false ~dockerfile `No_context))
  in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Current_web.run ~net ~mode site

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Check that the nginx container can serve a web page" in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner))

let () = exit @@ Cmd.eval cmd
