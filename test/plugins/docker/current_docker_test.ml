open Current.Syntax

let src = Logs.Src.create "test.docker" ~doc:"OCurrent test docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Fpath.t

module Image = struct
  type t = string
  let pp = Fmt.string
  let digest t = t
end

module Key = struct
  type t = {
    image : Image.t;
    cmd : string list;
  }

  let pp f { image; cmd } =
    Fmt.pf f "docker run %S @[%a@]" image Fmt.(list ~sep:sp (quote string)) cmd

  let compare = compare

  let digest = Fmt.to_to_string pp
end

let build_on platform ~src =
  Current.component "build-%s" platform |>
  let> c = src in
  Current.Primitive.const @@ Fmt.str "%s-image-%s" platform (Fpath.to_string c)

let build c =
  Current.component "build" |>
  let> c = c in
  Current.Primitive.const @@ "image-" ^ Fpath.to_string c

let build ?on src =
  match on with
  | None -> build src
  | Some platform -> build_on platform ~src


module Containers = Map.Make(Key)

let containers : (unit, [`Msg of string]) result Eio.Promise.u Containers.t ref = ref Containers.empty

module Run = struct
  type t = No_context
  module Key = Key
  module Value = Current.Unit

  let id = "docker-run"

  let pp = Key.pp

  let build No_context job (key : Key.t) =
    Current.Job.start job ~level:Current.Level.Average;
    let ready, set_ready = Eio.Promise.create () in
    containers := Containers.add key set_ready !containers;
    Current.Job.on_cancel job (fun m ->
        if not (Eio.Promise.is_resolved ready) then
          Eio.Promise.resolve set_ready @@ Error (`Msg m)
      );
    Eio.Promise.await ready

  let auto_cancel = true
end

module Run_cache = Current_cache.Make(Run)

let run_cache_ref : Run_cache.t option ref = ref None
let run_cache () =
  match !run_cache_ref with
  | Some c -> c
  | None -> failwith "Driver must construct caches before running"

let run image ~cmd =
  Current.component "docker run @[%a@]" Fmt.(list ~sep:sp string) cmd |>
  let> image = image in
  let key = { Key.image; cmd } in
  Run_cache.get (run_cache ()) No_context key

let complete image ~cmd r =
  let key = { Key.image; cmd } in
  match Containers.find_opt key !containers with
  | Some s -> Eio.Promise.resolve s r
  | None -> Fmt.failwith "Container %a not running!" Key.pp key

module Push = struct
  type t = No_context
  module Key = Image
  module Value = Current.Unit

  let id = "docker-push"

  let pp f k = Fmt.pf f "docker push %a" Key.pp k

  let build No_context job _key =
    Current.Job.start job ~level:Current.Level.Dangerous;
    Ok ()

  let auto_cancel = false
end

module Push_cache = Current_cache.Make(Push)

let push_cache_ref : Push_cache.t option ref = ref None
let push_cache () =
  match !push_cache_ref with
  | Some c -> c
  | None -> failwith "Driver must construct caches before pushing"

let push image ~tag =
  Current.component "docker push %s" tag |>
  let> image = image in
  Push_cache.get (push_cache ()) No_context image

let image_pulls
  : (string,
     string Current.or_error Eio.Promise.t *
     string Current.or_error Eio.Promise.u) Hashtbl.t
  = Hashtbl.create 5
let image_monitors = Hashtbl.create 5
let pulls_cond = Eio.Condition.create ()
let pulls_mutex = Eio.Mutex.create ()

(* Set by [Driver.test] from inside the engine's switch scope, so the mock's
   image-pull monitors can fork on the right switch. *)
let engine_sw_ref : Eio.Switch.t option ref = ref None
let set_engine_sw sw = engine_sw_ref := Some sw
let engine_sw () =
  match !engine_sw_ref with
  | Some sw -> sw
  | None -> failwith "Driver.test must run before pulling images in the docker mock"

let make_caches ~engine =
  let caps = Current_cache.caps_of_engine engine in
  run_cache_ref := Some (Run_cache.create ~caps);
  push_cache_ref := Some (Push_cache.create ~caps)

let get_pull tag =
  match Hashtbl.find_opt image_pulls tag with
  | Some x -> x
  | None ->
    let x = Eio.Promise.create () in
    Hashtbl.add image_pulls tag x;
    x

let image_monitor tag =
  match Hashtbl.find_opt image_monitors tag with
  | Some x -> x
  | None ->
    let read () = Eio.Promise.await (fst @@ get_pull tag) in
    let watch refresh =
      let stop = ref false in
      Eio.Fiber.fork_daemon ~sw:(engine_sw ()) (fun () ->
        let rec aux () =
          if !stop then `Stop_daemon
          else begin
            Eio.Mutex.use_rw ~protect:false pulls_mutex (fun () ->
              Eio.Condition.await pulls_cond pulls_mutex);
            refresh ();
            aux ()
          end
        in
        aux ()
      );
      fun () -> stop := true; Eio.Condition.broadcast pulls_cond
    in
    let pp f = Fmt.string f "docker pull" in
    let x = Current.Monitor.create ~sw:(engine_sw ()) ~read ~watch ~pp in
    Hashtbl.add image_monitors tag x;
    x

let pull tag =
  Current.component "docker pull %s" tag |>
  let> () = Current.return () in
  Current.Monitor.get (image_monitor tag)

let complete_pull tag image =
  match Hashtbl.find_opt image_pulls tag with
  | None -> Fmt.failwith "Image %S isn't being pulled!" tag
  | Some (_, set_image) -> Eio.Promise.resolve set_image image

let update_pull tag =
  Hashtbl.remove image_pulls tag;
  ignore @@ get_pull tag;
  Eio.Condition.broadcast pulls_cond

let reset () =
  containers := Containers.empty;
  Hashtbl.clear image_pulls;
  Hashtbl.clear image_monitors;
  Current_cache.Db.drop_all "docker-run";
  Current_cache.Db.drop_all "docker-push";
  run_cache_ref := None;
  push_cache_ref := None

let assert_finished () =
  !containers |> Containers.iter (fun key s ->
      let msg = Fmt.str "Container %a still running!" Key.pp key in
      (* If the container's resolver is still pending, the container wasn't
         finished — force-fail it and raise.  If resolve raises
         Invalid_argument, the promise was already resolved (good). *)
      if Eio.Promise.try_resolve s (Error (`Msg msg)) then
        failwith msg
    )
