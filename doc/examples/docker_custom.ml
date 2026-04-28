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

module Test = struct
  module Raw = Current_docker.Raw

  let id = "docker-custom-test"         (* A unique ID for the results database *)

  type t = No_context

  module Key = struct
    type t = {
      docker_context : string option;
      image : Raw.Image.t;
    }

    let digest { image; docker_context } =
      Yojson.Safe.to_string @@ `Assoc [
        "docker_context", [%derive.to_yojson:string option] docker_context;
        "image", `String (Raw.Image.hash image);
      ]

    let pp f t =
        Fmt.pf f "Test %a" Raw.Image.pp t.image
  end

  module Value = Current.Unit

  let run image = Raw.Cmd.docker ["container"; "run"; "-d"; Raw.Image.hash image]
  let exec id args = Raw.Cmd.docker ("container" :: "exec" :: "-i" :: id :: args)

  (* The test command to run. You might want to make this part of the key if it
     should be configurable. *)
  let test_command = ["curl"; "-Ss"; "--fail"; "http://localhost/"]

  let build No_context job { Key.docker_context; image } =
    Current.Job.start job ~level:Current.Level.Mostly_harmless;
    (* Start the container running: *)
    Raw.Cmd.with_container ~docker_context ~job ~kill_on_cancel:true (run image ~docker_context) @@ fun id ->
    Current.Job.log job "Waiting 1 second to let HTTP server start...";
    Eio.Time.sleep (Current.Job.clock job) 1.0;
    (* Test the container's service: *)
    Current.Process.exec ~cancellable:true ~job (exec id test_command ~docker_context)

  let auto_cancel = true

  let pp = Key.pp
end

module Test_cache = Current_cache.Make(Test)

module Docker = Current_docker.Default

(* Test a Docker image by running it and then execing curl inside it. *)
let test ~test_cache image =
  Current.component "test with@,@[<h>%a@]" Fmt.(list ~sep:sp string) Test.test_command |>
  let> image = image in
  let docker_context = Docker.docker_context in
  let image = Docker.Image.hash image |> Current_docker.Raw.Image.of_hash in
  Test_cache.get test_cache Test.No_context { Test.Key.docker_context; image }

(* Build a docker image with nginx and curl and then test it. *)
let pipeline ~docker ~test_cache () =
  let dockerfile =
    let+ base = Docker.pull docker ~schedule:weekly "nginx" in
    `Contents Dockerfile.(
        from (Docker.Image.hash base) @@
        run "apt-get update && apt-get install -y curl --no-install-recommends"
        |> string_of_t
      )
  in
  test ~test_cache (Docker.build docker ~pull:false ~dockerfile `No_context)

let main config mode =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let git = Current_git.create ~engine in
      let docker = Docker.create ~engine ~git in
      let test_cache = Test_cache.create ~caps:(Current_cache.caps_of_engine engine) in
      pipeline ~docker ~test_cache ())
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
