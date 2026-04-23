open Current.Result.Syntax
open Auth

type t = auth option

let id = "docker-push-manifest"

(* Pushing can fail with "malformed MIME header line: Too Many Requests (HAP429)",
   so limit to one at a time. *)
let push_mutex = Eio.Mutex.create ()

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

let create_cmd ~config ~tag {Value.manifests} =
  Cmd.docker ~config ~docker_context:None (["manifest"; "create"; tag] @ manifests)

let push_cmd ~config tag =
  Cmd.docker ~config ~docker_context:None ["manifest"; "push"; tag]

let or_fail = function
  | Ok x -> x
  | Error (`Msg x) -> failwith x

let publish auth job tag value =
  Current.Job.start job ~level:Current.Level.Dangerous;
  Current.Process.with_tmpdir ~prefix:"push-manifest" @@ fun config ->
  Bos.OS.File.write Fpath.(config / "config.json") {|{"experimental": "enabled"}|} |> or_fail;
  let* () = Auth.login ~config ~docker_context:None ~job auth in
  Prometheus.Gauge.inc_one Metrics.docker_push_manifest_events;
  Fun.protect
    ~finally:(fun () -> Prometheus.Gauge.dec_one Metrics.docker_push_manifest_events)
    (fun () ->
      let* () = Current.Process.exec ~cancellable:true ~job (create_cmd ~config ~tag value) in
      Eio.Mutex.use_rw ~protect:false push_mutex @@ fun () ->
      let* output = Current.Process.check_output ~cancellable:true ~job (push_cmd ~config tag) in
      (* docker-manifest is still experimental and doesn't have a sensible output format yet. *)
      Current.Job.write job output;
      let output = String.trim output in
      let hash =
        match Astring.String.cut ~rev:true ~sep:"\n" output with
        | None -> output
        | Some (_, id) -> id
      in
      let repo_id = Printf.sprintf "%s@%s" tag hash in
      Current.Job.log job "--> %S" repo_id;
      Ok repo_id)

let pp f (tag, value) =
  Fmt.pf f "push %s = %s" tag (Value.digest value)

let auto_cancel = true
