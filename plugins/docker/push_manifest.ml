open Lwt.Infix

open Auth

type t = auth option

let ( >>!= ) = Lwt_result.bind

let id = "docker-push-manifest"

(* Pushing can fail with "malformed MIME header line: Too Many Requests (HAP429)",
   so limit to one at a time. *)
let push_mutex = Lwt_mutex.create ()

module Key = Current.String

module Value = struct
  type t = {
    manifests : S.repo_id list;
  }

  let digest { manifests } =
    Yojson.Safe.to_string @@ `Assoc [
      "manifests", `List (List.map (fun id -> `String id) manifests);
    ]
end

module Outcome = struct
  include Current.String

  let unmarshal = function
    | "()" -> failwith "Result from old version. Need rebuild"
    | repo_id -> repo_id
end

(* [imagetools create] assembles and pushes in one step, and accepts index
   source references (e.g. a single-platform image BuildKit wrapped with an
   attestation), which [docker manifest create] rejects. *)
let create_cmd ~config ~tag {Value.manifests} =
  Cmd.docker ~config ~docker_context:None
    (["buildx"; "imagetools"; "create"; "-t"; tag] @ manifests)

(* [create] doesn't report the pushed digest, so read it back. The [printf]
   wrapper is required: buildx prints its default block for a bare
   [{{.Manifest.Digest}}]. *)
let inspect_cmd ~config ~tag =
  Cmd.docker ~config ~docker_context:None
    ["buildx"; "imagetools"; "inspect"; "--format"; {|{{printf "%s" .Manifest.Digest}}|}; tag]

let publish auth job tag value =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  Current.Process.with_tmpdir ~prefix:"push-manifest" @@ fun config ->
  (* [login] writes the credentials buildx reads from [config]. *)
  Auth.login ~config ~docker_context:None ~job auth >>!= fun () ->
  Prometheus.Gauge.inc_one Metrics.docker_push_manifest_events;
  (* [create] performs the push, so serialise it. *)
  (Lwt_mutex.with_lock push_mutex @@ fun () ->
   Current.Process.exec ~cancellable:true ~job (create_cmd ~config ~tag value))
  >>= (function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    Current.Process.check_output ~cancellable:true ~job (inspect_cmd ~config ~tag) >>!= fun output ->
    let hash = String.trim output in
    let repo_id = Printf.sprintf "%s@%s" tag hash in
    Current.Job.log job "--> %S" repo_id;
    Lwt_result.return repo_id)
  >|= (fun res -> Prometheus.Gauge.dec_one Metrics.docker_push_manifest_events; res)

let pp f (tag, value) =
  Fmt.pf f "push %s = %s" tag (Value.digest value)

let auto_cancel = true
