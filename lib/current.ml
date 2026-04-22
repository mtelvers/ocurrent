type 'a or_error = ('a, [`Msg of string]) result

module Result = Current_term.Result

module Config = Config

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "core"

  let evaluation_time_seconds =
    let help = "Total time spent evaluating" in
    Summary.v ~help ~namespace ~subsystem "evaluation_time_seconds"

  let pipeline_stage_total =
    let help = "Number of pipeline stages by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "pipeline_stage_total"
end

type job_id = string

class type actions = object
  method pp : Format.formatter -> unit
  method rebuild : (unit -> job_id) option
end

module Metadata = struct
  type t = {
    job_id : job_id option;
    update : Current_term.Output.active option;
  }
end

include Current_term.Make(Metadata)

module Primitive = struct
  type 'a t = 'a primitive

  let const x = Current_incr.const (Ok x, None)

  let map_result fn t =
    Current_incr.of_cc begin
      Current_incr.read t @@ fun (x, md) ->
      let y = try fn x with ex -> Error (`Msg (Printexc.to_string ex)) in
      Current_incr.write (y, md)
    end
end

type 'a term = 'a t

module Engine = struct
  (* Active jobs are ones which are referenced by the active pipeline.
     These are the only ones which can have actions attached. *)
  let active_jobs : actions list Job.Map.t ref = ref Job.Map.empty

  module Step = struct
    type t = < >
    let create () = object end
    let equal = (=)
    let current_step = ref (create ())
    let now () = !current_step
    let advance () =
      current_step := create ()
  end

  type results = {
    value : unit Current_term.Output.t;
    jobs : actions Job.Map.t;
  }

  type t = {
    last_result : results ref;
    pipeline : unit term Lazy.t;
    config : Config.t;
  }

  let release_queue = Queue.create ()

  let rec flush_release_queue () =
    match Queue.take_opt release_queue with
    | None -> ()
    | Some fn ->
      fn ();
      flush_release_queue ()

  (* Signal that the next evaluation should start. One producer (update) plus
     the engine loop as consumer. We use a mutable promise/resolver pair so
     that [update] can be called from any fiber and its effect is observed by
     the engine's next iteration. *)
  let next_eval_mu = Eio.Mutex.create ()
  let next_eval_ref : (unit Eio.Promise.t * unit Eio.Promise.u) option ref = ref None

  let next_evaluation () =
    Eio.Mutex.use_rw ~protect:false next_eval_mu @@ fun () ->
    match !next_eval_ref with
    | Some (p, _) -> p
    | None ->
      let p, r = Eio.Promise.create () in
      next_eval_ref := Some (p, r);
      p

  let update () =
    let to_resolve =
      Eio.Mutex.use_rw ~protect:false next_eval_mu @@ fun () ->
      match !next_eval_ref with
      | None -> None
      | Some (_, r) ->
        next_eval_ref := None;
        Some r
    in
    Option.iter (fun r -> Eio.Promise.resolve r ()) to_resolve

  let booting = {
    value = Error (`Active `Running);
    jobs = Job.Map.empty;
  }

  let default_trace ~next:_ _ = ()

  let pipeline t = Lazy.force t.pipeline

  let create ?(config=Config.default) ?(trace=default_trace) f =
    let last_result = ref booting in
    let pipeline = lazy (f ()) in
    let t = { last_result; config; pipeline } in
    let rec aux outcome =
      let next = next_evaluation () in
      Log.debug (fun f -> f "Evaluating...");
      let t0 = Unix.gettimeofday () in
      Current_incr.propagate ();
      let t1 = Unix.gettimeofday () in
      Prometheus.Summary.observe Metrics.evaluation_time_seconds (t1 -. t0);
      flush_release_queue ();
      let r = Current_incr.observe outcome in
      if not (Current_term.Output.equal Unit.equal r !last_result.value) then
        Log.info (fun f -> f "Result: %a" Current_term.(Output.pp Fmt.(any "()")) r);
      last_result := {
        value = r;
        jobs = Job.Map.map List.hd !active_jobs;
      };
      trace ~next !last_result;
      Log.debug (fun f -> f "Waiting for an external event...");
      Eio.Promise.await next;
      Eio.Fiber.yield ();
      Step.advance ();
      aux outcome
    in
    if Current_incr.observe Config.now <> None then
      failwith "Engine is already running (Config.now already set)!";
    Current_incr.change Config.active_config (Some config);
    Eio.Fiber.fork ~sw:(Engine_env.get_sw ()) (fun () ->
      Fun.protect
        ~finally:(fun () -> Current_incr.change Config.active_config None)
        (fun () ->
          Eio.Fiber.yield ();
          try aux (Executor.run (Lazy.force pipeline))
          with Exit ->
            (* Clean up, for unit-tests *)
            Current_incr.propagate ();
            flush_release_queue ();
            raise Exit));
    t

  let on_disable fn =
    Current_incr.on_release @@ fun () ->
    Queue.add fn release_queue

  let state t = !(t.last_result)

  let jobs s = s.jobs

  let config t = t.config

  let update_metrics _t =
    let { Current_term.S.ok; waiting_for_confirmation; ready; running; failed; blocked } = Analysis.quick_stat () in
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "ok") (float_of_int ok);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "waiting_for_confirmation") (float_of_int waiting_for_confirmation);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "ready") (float_of_int ready);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "running") (float_of_int running);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "failed") (float_of_int failed);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "blocked") (float_of_int blocked)
end

module Var (T : Current_term.S.T) = struct
  type t = {
    current : T.t Current_term.Output.t Current_incr.var;
    name : string;
  }

  let create ~name current =
    let current = Current_incr.var current in
    { current; name }

  let get t =
    let open Syntax in
    component "%s" t.name |>
    let> () = return () in
    Current_incr.of_cc begin
      Current_incr.read (Current_incr.of_var t.current) @@ fun v ->
      Current_incr.write (v, None)
    end

  let set t v =
    Current_incr.change t.current v;
    Engine.update ()

  let update t f =
    Current_incr.change t.current (f (Current_incr.observe (Current_incr.of_var t.current)));
    Engine.update ()
end

module Monitor = struct
  type 'a t = {
    read : unit -> 'a or_error;
    watch : (unit -> unit) -> (unit -> unit);
    pp : Format.formatter -> unit;
    value : 'a Current_term.Output.t Current_incr.var;
    reading : bool Current_incr.var;
    mutable ref_count : int;
    mutable need_refresh : bool;
    mutable active : bool;
    cond : Eio.Condition.t;
    mutex : Eio.Mutex.t;
  }

  let catch t fn =
    try fn ()
    with ex ->
      Log.warn (fun f -> f "Uncaught exception in monitor %t: %a" t.pp Fmt.exn ex);
      Error (`Msg (Printexc.to_string ex))

  let refresh t () =
    t.need_refresh <- true;
    Eio.Condition.broadcast t.cond

  let rec enable t =
    let unwatch = t.watch (refresh t) in
    if t.ref_count = 0 then disable ~unwatch t
    else get_value ~unwatch t
  and disable ~unwatch t =
    unwatch ();
    if t.ref_count > 0 then enable t
    else (
      assert t.active;
      t.active <- false;
      Current_incr.change t.value @@ Error (`Active `Running);
      `Finished
    )
  and get_value ~unwatch t =
    t.need_refresh <- false;
    Current_incr.change t.reading true;
    Engine.update ();
    let v = catch t t.read in
    Current_incr.change t.reading false;
    Current_incr.change t.value (v :> _ Current_term.Output.t);
    Engine.update ();
    wait ~unwatch t
  and wait ~unwatch t =
    if t.ref_count = 0 then disable ~unwatch t
    else if t.need_refresh then get_value ~unwatch t
    else begin
      Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        Eio.Condition.await t.cond t.mutex);
      wait ~unwatch t
    end

  let get t =
    Current_incr.of_cc begin
      t.ref_count <- t.ref_count + 1;
      Engine.on_disable (fun () ->
          assert (t.ref_count > 0);
          t.ref_count <- t.ref_count - 1;
          if t.ref_count = 0 then Eio.Condition.broadcast t.cond
        );
      if not t.active then (
        t.active <- true;
        Eio.Fiber.fork ~sw:(Engine_env.get_sw ()) (fun () ->
            Eio.Fiber.yield ();
            let `Finished = enable t in
            ()
          )
      );
      Current_incr.read (Current_incr.of_var t.value) @@ fun value ->
      Current_incr.read (Current_incr.of_var t.reading) @@ fun reading ->
      let update = if reading then Some `Running else None in
      let metadata = { Metadata.job_id = None; update } in
      Current_incr.write (value, Some metadata)
    end

  let create ~read ~watch ~pp =
    {
      ref_count = 0;
      active = false;
      need_refresh = true;
      cond = Eio.Condition.create ();
      mutex = Eio.Mutex.create ();
      reading = Current_incr.var false;
      value = Current_incr.var (Error (`Active `Running));
      read; watch; pp
    }
end

module Level = Level

module String = struct
  type t = string
  let digest t = t
  let pp = Fmt.string
  let marshal t = t
  let unmarshal t = t
  let equal = String.equal
end

module Unit = struct
  type t = unit

  let pp f () = Fmt.string f "()"
  let compare () () = 0
  let digest () = ""
  let equal () () = true
  let marshal () = "()"
  let unmarshal = function
    | "()" -> ()
    | x -> Fmt.failwith "Unit.unmarshal(%S)" x
end

let state_dir = Disk_store.state_dir

module Db = Db
module Process = Process
module Switch = Switch
module Pool = Pool
module Log_matcher = Log_matcher

module Job = struct
  include Job

  let register_actions job_id actions =
    let add = function
      | None -> Some [actions]
      | Some xs -> Some (actions :: xs)
    in
    let remove xs =
      let rec aux = function
        | [] -> assert false
        | x :: xs when x == actions -> xs
        | x :: xs -> x :: aux xs
      in
      match aux (Option.get xs) with
      | [] -> None
      | xs -> Some xs
    in
    Engine.active_jobs := Job.Map.update job_id add !Engine.active_jobs;
    Engine.on_disable @@ fun () ->
    Engine.active_jobs := Job.Map.update job_id remove !Engine.active_jobs
end
