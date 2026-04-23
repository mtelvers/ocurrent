open Current.Result.Syntax

type t = No_context

module Key = struct
  type t = {
    docker_context : string option;
    arch: string;
    tag : string;
  } [@@deriving to_yojson]

  let cmd { docker_context; tag; _ } = Cmd.docker ~docker_context ["manifest"; "inspect"; "--"; tag]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Current.String

let id = "docker-peek"

let build No_context job key =
  Current.Job.start job ~level:Current.Level.Mostly_harmless;
  let { Key.docker_context = _; tag; arch } = key in
  Prometheus.Gauge.inc_one Metrics.docker_peek_events;
  Fun.protect
    ~finally:(fun () -> Prometheus.Gauge.dec_one Metrics.docker_peek_events)
    (fun () ->
      let* manifest = Current.Process.check_output ~cancellable:true ~job (Key.cmd key) in
      match Pull.get_digest_from_manifest manifest arch with
      | Error _ as e -> e
      | Ok hash ->
        Current.Job.log job "Got %S" hash;
        Ok (tag ^ "@" ^ hash))

let pp f key = Cmd.pp f (Key.cmd key)

let auto_cancel = true
