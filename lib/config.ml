type t = {
  mutable confirm : Level.t option;
  level_cond : Eio.Condition.t;
  level_mutex : Eio.Mutex.t;
  auto_release : Duration.t option;
}

let set_confirm t level =
  Log.info (fun f -> f "Confirmation threshold is now %a" (Fmt.Dump.option Level.pp) level);
  t.confirm <- level;
  Eio.Condition.broadcast t.level_cond

let get_confirm t = t.confirm

(* If the level isn't changed manually within [duration], remove limiter.
   The Engine is responsible for calling this once it has a switch to fork on. *)
let start_slow_start ~sw ~clock t =
  match t.auto_release with
  | None -> ()
  | Some duration ->
    Eio.Fiber.fork ~sw (fun () ->
      let result =
        Eio.Fiber.first
          (fun () ->
            Eio.Time.sleep clock (Duration.to_f duration);
            `Timeout)
          (fun () ->
            Eio.Mutex.use_ro t.level_mutex (fun () ->
              Eio.Condition.await t.level_cond t.level_mutex);
            `Changed)
      in
      match result with
      | `Timeout ->
        Log.info (fun f -> f "Slow start period over; removing limiter");
        set_confirm t None
      | `Changed -> ()
    )

let v ?auto_release ?confirm () =
  let level_cond = Eio.Condition.create () in
  let level_mutex = Eio.Mutex.create () in
  { confirm; level_cond; level_mutex; auto_release }

let default = v ()

let active_config : t option Current_incr.var = Current_incr.var None

let now = Current_incr.of_var active_config

let rec confirmed l t =
  match t.confirm with
  | Some threshold when Level.compare l threshold >= 0 ->
    Eio.Mutex.use_ro t.level_mutex (fun () ->
      Eio.Condition.await t.level_cond t.level_mutex);
    confirmed l t
  | _ ->
    ()

open Cmdliner

let cmdliner_confirm =
  let levels = List.map (fun l -> Level.to_string l, Some l) Level.values in
  let enum = ("none", None) :: levels in
  let doc =
    Fmt.str
      "Confirm before starting operations at or above this level (%s)."
      (Arg.doc_alts_enum enum)
  in
  Arg.opt (Arg.enum enum) None @@
  Arg.info ~doc ["confirm"]

let auto_release =
  Arg.value @@
  Arg.(opt (some int)) None @@
  Arg.info
    ~doc:"Remove confirm threshold after this many seconds from start-up."
    ~docv:"SEC"
    ["confirm-auto-release"]

let cmdliner =
  let make auto_release confirm =
    let auto_release = Option.map Duration.of_sec auto_release in
    v ?auto_release ?confirm () in
  Term.(const make $ auto_release $ Arg.value cmdliner_confirm)
