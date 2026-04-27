module Job = Current.Job

(** Per-engine capabilities for cache instances: the parent switch, the
    capabilities a cache slot needs to construct {!Current.Job.t}s, and the
    engine-wide registry of in-flight jobs. Built once via
    {!caps_of_engine} and shared across every {!Make}/{!Output}/{!Generic}
    cache the plugin or pipeline constructs. *)
type caps = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  cache_registry : Current.Engine.cache_registry;
}

let caps_of_engine engine = {
  sw = Current.Engine.switch engine;
  clock = Current.Engine.clock engine;
  process_mgr = Current.Engine.process_mgr engine;
  fs = Current.Engine.fs engine;
  cache_registry = Current.Engine.cache_registry engine;
}

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "cache"

  let memory_cache_items =
    let help = "Number of results cached in RAM" in
    Gauge.v_label ~label_name:"id" ~help ~namespace ~subsystem "memory_cache_items"

  let evaluations_total =
    let help = "Number of evaluations performed" in
    Counter.v ~help ~namespace ~subsystem "evaluations_total"
end

module Schedule = struct
  type t = {
    valid_for : Duration.t option;
  }

  let v ?valid_for () = { valid_for }

  let default = v ()
end

let durations = Duration.[
  to_year, "years";
  to_day, "days";
  to_hour, "hours";
  to_min, "minutes";
  to_sec, "seconds"
]

let pp_duration_rough f d =
  let rec aux = function
    | [] -> Fmt.pf f "%.3f s" (Duration.to_f d)
    | (to_int, units) :: xs ->
      let i = to_int d in
      if i < 5 then aux xs
      else Fmt.pf f "%d %s" i units
  in
  aux durations

module Generic(Op : S.GENERIC) = struct
  module Value : sig
    (** Cache the value of the digest. *)

    type t

    val v : Op.Value.t -> t
    val value : t -> Op.Value.t
    val digest : t -> string
    val equal : t -> t -> bool
  end = struct
    type t = {
      value : Op.Value.t;
      digest : string;
    }

    let v value = { value; digest = Op.Value.digest value }
    let value t = t.value
    let digest t = t.digest
    let equal a b = a.digest = b.digest
  end

  module Instance = struct
    (** An Instance manages a single key. *)

    type op = {
      value : Value.t;                (* The value currently being set. *)
      job : Job.t;
      mutable autocancelled : bool;   (* This op is expected to fail *)
    }

    type latched = (Op.Outcome.t, [`Msg of string]) result option
    (** A previous outcome that can still be used while rebuilding. *)

    type t = {
      caps : caps;                          (* Capabilities for jobs and the registry. *)
      key : Op.Key.t;
      mutable build_number : int64;         (* Number of recorded (incl failed) builds with this key. *)
      mutable ref_count : int;              (* The number of watchers waiting for the result (for auto-cancel). *)
      mutable last_set : Current.Engine.Step.t; (* Last evaluation step setting this instance. *)
      mutable job_id : Current.job_id option; (* Current or last log *)
      mutable current : string option;      (* The current digest value, if known. *)
      mutable desired : Value.t;            (* The value we want. *)
      mutable ctx : Op.t;                   (* The context for [desired]. *)
      mutable op : [
        | `Active of op * latched           (* The currently-running operation. *)
        | `Finished of (Op.Outcome.t, [`Msg of string]) result
        | `Retry of latched                 (* Need to try again. *)
      ];
      mutable mtime : float;                (* Time last operation completed (if finished or error). *)
      mutable expires : (float * (unit -> unit)) option;  (* Time and cancel function. *)
      notify : unit Current_incr.var;       (* Async thread sets this to update results. *)
      release : unit -> unit;               (* Remove [t] from cache (call when inactive and ref-count = 0). *)
      slot_sw : Eio.Switch.t;               (* Lifetime of this slot. Parent of all per-slot fibers
                                               (job runner, expiry timer). Closed by [release]. *)
    }

    (* State model:

       The first time a key is used, a new instance is created in the Retry state.
       Whenever an instance is wanted and we are in Retry, we transition to Active
       and start the Lwt builder process. The only way to leave Active is by the
       Lwt process finishing.

       While Active, we may flag that the value we are setting is out-of-date,
       that the user wants to cancel, or that we should cancel because the build
       is no longer needed. But we still wait for the process to finish,
       possibly encouraging it using [Job.cancel].

       When an Active job finishes:

       - If it was auto-cancelled then we discard the result and return to Retry.
       - Otherwise we store the result on disk.
       - If we need to rebuild (because the user asked for another value during the build)
         then we return to Retry.
       - Otherwise, we move to Finished or Error, depending on whether the operation succeeded.

       In Finished or Error, the user can trigger a rebuild, moving us back to Retry.
       Also, if the user sets a different value then we move to Retry.

       The user can only ask to cancel while we are Active. They can only ask to Rebuild
       when Finished or Error.

       If we reach the scheduled time for a rebuild while in Finished or Error then
       we also move to Retry, but we also continue reporting the previous result
       while rebuilding. *)

    let pp_op f (k, v) = Op.pp f (k, Value.value v)
    let pp_desired f t = pp_op f (t.key, t.desired)

    let pp f t =
      match t.op with
      | `Finished (Error (`Msg msg)) ->
        Fmt.pf f "%a: %s" pp_desired t msg
      | `Finished (Ok _) ->
        Fmt.pf f "%a (completed)" pp_desired t
      | `Retry _ ->
        Fmt.pf f "%a (retry scheduled)" pp_desired t
      | `Active (op, _) ->
        if Value.equal op.value t.desired then
          Fmt.pf f "%a (in-progress)" pp_op (t.key, op.value)
        else
          Fmt.pf f "%a (stale), then %a"
            pp_op (t.key, op.value)
            pp_desired t

    (* Caller needs to notify about the change, if needed. *)
    let invalidate t =
      match t.op with
      | `Retry _ ->
        t.current <- None;
        Db.invalidate ~op:Op.id (Op.Key.digest t.key)
      | _ -> assert false

    let notify t =
      Current_incr.change t.notify () ~eq:(fun _ _ -> false);
      Current.Engine.update ()

    (* Spawn a switch as a child of [parent_sw]. The new switch lives until
       [release_r] is resolved; even after that, [Switch.run] still waits
       for any child fibers before closing. Returns the switch and the
       resolver that closes it.

       Building block for per-slot switches (parent = engine sw) and
       per-timer sub-switches (parent = slot_sw). *)
    let spawn_managed_switch ~parent_sw =
      let sw_p, sw_r = Eio.Promise.create () in
      let release_p, release_r = Eio.Promise.create () in
      Eio.Fiber.fork_daemon ~sw:parent_sw (fun () ->
        Eio.Switch.run (fun sw ->
          Eio.Promise.resolve sw_r sw;
          Eio.Promise.await release_p);
        `Stop_daemon);
      Eio.Promise.await sw_p, release_r

    let spawn_slot_switch ~caps = spawn_managed_switch ~parent_sw:caps.sw

    (* If [t] isn't in (or moving to) the desired state, start a thread to do that,
       unless we already tried that and failed. Only call this if the instance is currently
       wanted. If called from an async thread, you must call [notify] afterwards too. *)
    let rec maybe_start ~config t =
      assert (t.ref_count > 0);
      match t.op with
      | `Finished (Error _) -> () (* Wait for error to be cleared. *)
      | `Active _ when not Op.auto_cancel ->
        (* Already running something and we don't auto-cancel.
           When the stale job completes, we'll get called again. *)
        ()
      | `Active (op, _) when Value.equal t.desired op.value ->
        (* We're already working to set the desired value. Just keep going. *)
        ()
      | `Active (op, _) ->
        assert Op.auto_cancel;
        Log.info (fun f -> f "Auto-cancelling %a" pp_op (t.key, op.value));
        op.autocancelled <- true;
        (* Cancel existing job. When that finishes, we'll get called again. *)
        Job.cancel op.job "Auto-cancelling job because it is no longer needed"
      | `Retry latched ->
        publish ~latched ~config t
      | `Finished outcome ->
        match t.current with
        | Some current when current = Value.digest t.desired -> () (* Already the desired value. *)
        | _ ->
          (* Either we don't know the current state, or we know we want something different.
             We're not already running, and we haven't already failed. Time to publish! *)
          let latched = if Op.latched then Some outcome else None in
          publish ~latched ~config t
    and maybe_restart ~config t =
      maybe_start ~config t;
      notify t
    and publish ~latched ~config t =
      (* Once we start publishing, we don't know the state (it might be better to
         wait until the job starts before doing this): *)
      t.current <- None;
      let ctx = t.ctx in
      let priority = if latched = None then `High else `Low in
      let key_digest = Op.Key.digest t.key in
      let job_id_ref = ref None in
      let registry = t.caps.cache_registry in
      Eio.Fiber.fork ~sw:t.slot_sw (fun () ->
        Fun.protect
          (fun () ->
             Eio.Switch.run (fun job_sw ->
               let job =
                 Job.create ~priority ~sw:job_sw
                   ~clock:t.caps.clock
                   ~process_mgr:t.caps.process_mgr
                   ~fs:t.caps.fs
                   ~label:Op.id ~config ()
               in
               let job_id = Job.id job in
               job_id_ref := Some job_id;
               t.job_id <- Some job_id;
               let op = { value = t.desired; job; autocancelled = false } in
               let ready = Eio.Time.now t.caps.clock |> Unix.gmtime in
               t.op <- `Active (op, latched);
               let pp_op f = pp_op f (t.key, op.value) in
               Job.log job "New job: %t" pp_op;
               Eio.Fiber.fork ~sw:t.slot_sw (fun () ->
                 let _ = Eio.Promise.await (Job.start_time job) in
                 Eio.Fiber.yield ();        (* Ensure we're outside any propagate *)
                 notify t
               );
               Hashtbl.add registry.key_of_job_id job_id (Op.id, key_digest);
               Hashtbl.add registry.job_id_of_key (Op.id, key_digest) job_id;
               let outcome =
                 try Op.run ctx job t.key (Value.value op.value)
                 with ex -> Error (`Msg (Printexc.to_string ex))
               in
               Eio.Fiber.yield ();        (* Ensure we're outside any propagate *)
               let end_time = Unix.gmtime @@ Eio.Time.now t.caps.clock in
               if op.autocancelled then (
                 t.op <- `Retry latched;
                 invalidate t
               ) else (
                 (* Record the result *)
                 let running =
                   match Eio.Promise.peek (Job.start_time job) with
                   | Some x -> Some (Unix.gmtime x)
                   | None when Stdlib.Result.is_ok outcome -> Fmt.failwith "Job.start not called!";
                   | None -> None
                 in
                 let outcome =
                   match Current.Job.cancelled_state op.job, outcome with
                   | Error (`Msg msg), _ ->
                     Job.log job "%s" msg;
                     Error (`Msg "Cancelled")
                   | Ok (), Ok _ -> Job.log job "Job succeeded"; outcome
                   | Ok (), Error (`Msg m) ->
                     Job.log job "Job failed: %s" m;
                     match Current.Log_matcher.analyse_job job with
                     | None -> outcome
                     | Some e -> Error (`Msg e)
                 in
                 t.mtime <- Eio.Time.now t.caps.clock;
                 Db.record ~op:Op.id ~job_id
                   ~key:key_digest
                   ~value:(Value.digest op.value)
                   ~ready ~running ~finished:end_time
                   ~build:t.build_number
                   (Stdlib.Result.map Op.Outcome.marshal outcome);
                 t.build_number <- Int64.succ t.build_number;
                 match outcome with
                 | Ok outcome ->
                   t.current <- Some (Value.digest op.value);
                   t.op <- `Finished (Ok outcome);
                 | Error e ->
                   if Value.equal op.value t.desired then (
                     t.op <- `Finished (Error e)
                   ) else (
                     (* It failed, but we have a new value to set: ignore the stale error. *)
                     t.op <- `Retry None;
                     invalidate t
                   )
               )))
          ~finally:(fun () ->
             (match !job_id_ref with
              | Some job_id ->
                Hashtbl.remove registry.key_of_job_id job_id;
                Hashtbl.remove registry.job_id_of_key (Op.id, key_digest)
              | None -> ());
             (* While we were working, we might have decided we wanted something else.
                If so, start that now. *)
             if t.ref_count > 0 then maybe_restart ~config t
             else t.release ()
          )
      )

    let limit_expires t time =
      let set () =
        let remaining_time = time -. Eio.Time.now t.caps.clock in
        (* Each timer gets its own switch under [t.slot_sw]. Cancelling
           the timer = closing its switch (resolving [release]). The fiber
           below either runs to completion (timer fired) or is cancelled
           via the switch. *)
        let timer_sw, release = spawn_managed_switch ~parent_sw:t.slot_sw in
        Eio.Fiber.fork ~sw:timer_sw (fun () ->
          try
            if remaining_time > 0.0 then Eio.Time.sleep t.caps.clock remaining_time;
            Eio.Fiber.yield ();        (* Ensure we're outside any propagate *)
            t.expires <- None;
            Log.info (fun f -> f "Result for %a has expired" pp_desired t);
            let latched =
              match t.op with
              | `Finished x -> Some x
              | `Retry x -> x
              | _ -> None
            in
            t.op <- `Retry latched;
            t.current <- None;
            (match Current_incr.observe Current.Config.now with
             | Some config -> maybe_restart ~config t
             | None -> Log.warn (fun f -> f "Can't trigger restart as config is now None (shutting down?)"));
            (* Close the timer switch now that we've finished firing. *)
            (try Eio.Promise.resolve release () with _ -> ())
          with
          | Eio.Cancel.Cancelled _ -> ()
          | ex ->
            Log.err (fun f -> f "Expiry thread failed: %a" Fmt.exn ex)
        );
        let cancel () =
          try Eio.Promise.resolve release () with _ -> ()
        in
        t.expires <- Some (time, cancel)
      in
      match t.expires with
      | Some (prev, _) when prev <= time -> ()            (* Already expiring by [time] *)
      | Some (_, cancel) -> cancel (); set ()
      | None -> set ()

    let cancel_expires t =
      match t.expires with
      | Some (_, cancel) -> cancel (); t.expires <- None
      | None -> ()

    (* Create a new in-memory instance, initialising it from the database. *)
    let load ~caps ~release ctx key desired =
      let slot_sw, slot_release_r = spawn_slot_switch ~caps in
      let cleaned = ref false in
      let do_cleanup () =
        if not !cleaned then begin
          cleaned := true;
          (* Original [release]: removes the Instance from the map and
             decrements the prometheus gauge. Wrap in [try] because the
             switch's on_release may fire after the assertion in
             [release] would no longer hold. *)
          (try release () with _ -> ())
        end
      in
      (* Always run cleanup when the slot switch closes — whether via
         normal [release] (resolves [slot_release_r]) or via the engine
         switch being cancelled (cancellation propagates here). *)
      Eio.Switch.on_release slot_sw do_cleanup;
      let release () =
        do_cleanup ();
        Eio.Promise.resolve slot_release_r ()
      in
      let step_id = Current.Engine.Step.now () in
      let current, job_id, op, mtime, build_number =
        match Db.lookup ~op:Op.id (Op.Key.digest key) with
        | Some { Db.value; job_id; outcome; finished; build; rebuild; _ } ->
          let current, op = match outcome with
            | _ when rebuild -> None, `Retry None
            | Error e when value <> Value.digest desired ->
              (* The saved state was an error, but the desired value has changed, so clear it. *)
              let latched = if Op.latched then Some (Error e) else None in
              Some value, `Retry latched
            | outcome ->
              try Some value, `Finished (Result.map Op.Outcome.unmarshal outcome)
              with ex ->
                Log.warn (fun f -> f "Failed to restore %S cached outcome: %a (will rebuild)" Op.id Fmt.exn ex);
                Db.invalidate ~op:Op.id (Op.Key.digest key);
                None, `Retry None
          in
          current, Some job_id, op, finished, Int64.succ build
        | None -> None, None, `Retry None, Unix.gettimeofday (), 0L
      in
      let notify = Current_incr.var () in
      { caps; key; current; desired; ctx; op; job_id; last_set = step_id;
        ref_count = 0; release; mtime; build_number; notify; expires = None;
        slot_sw }

    (* Register the actions for a resolved (non-active) instance.
       Report it as changed when a rebuild is requested (manually or via the schedule). *)
    let register_resolved t ~schedule ~config =
      let key = t.key in
      match t.job_id with
      | None -> assert false
      | Some job_id ->
        let rebuild () =
          match t.op with
          | `Finished _ | `Retry _ ->
            t.op <- `Retry None;
            invalidate t;
            maybe_restart ~config t;
            Option.get t.job_id
          | `Active _ ->
            Log.info (fun f -> f "Rebuild(%a): already rebuilding" pp_op (key, t.desired));
            Option.get t.job_id
        in
        match schedule.Schedule.valid_for with
        | None ->
          Current.Job.register_actions job_id @@ object
            method pp f = pp f t
            method rebuild = Some rebuild
          end
        | Some duration ->
          let expires = Duration.to_f duration +. t.mtime in
          limit_expires t expires;
          Current.Job.register_actions job_id @@
          object
            method pp f =
              let remaining_time = expires -. Eio.Time.now t.caps.clock in
              if remaining_time <= 0.0 then
                Fmt.pf f "%a (expired)" pp_op (key, t.desired)
              else
                Fmt.pf f "%a will be invalid after %a"
                  pp_op (key, t.desired)
                  pp_duration_rough (Duration.of_f remaining_time)
            method rebuild = Some rebuild
          end

    let register_actions ~schedule ~config t =
      match t.op with
      | `Finished _ | `Retry _ -> register_resolved ~schedule ~config t
      | `Active _ ->
        cancel_expires t;
        Current.Job.register_actions (Option.get t.job_id) @@ object
          method pp f = pp f t
          method rebuild = None
        end

    let update ~value ~ctx t =
      let step_id = Current.Engine.Step.now () in
      let changed = not (Value.equal value t.desired) in
      if t.last_set = step_id then (
        if changed then
          Fmt.failwith "Error: instance %a set to different values in the same step!" pp_op (t.key, value);
      ) else (
        t.last_set <- step_id;
        t.ctx <- ctx;
        if changed then (
          t.desired <- value;
          (* Clear any error when the desired value changes: *)
          match Op.latched, t.op with
          | _, (`Active _ | `Finished (Ok _)) -> ()
          | true, `Retry _ -> ()
          | true, `Finished (Error x) -> t.op <- `Retry (Some (Error x));
          | false, (`Finished (Error _) | `Retry _ )-> t.op <- `Retry None
        );
      )

    let run ~config ~schedule t =
      t.ref_count <- t.ref_count + 1;
      Current.Engine.on_disable (fun () ->
          (* This is called at the end of a propagation if we're no longer needed. *)
          t.ref_count <- t.ref_count - 1;
          if t.ref_count = 0 then (
            match t.op with
            | `Finished _ | `Retry _ ->
              cancel_expires t;
              t.release ()
            | `Active (op, _) ->
              if Op.auto_cancel then (
                op.autocancelled <- true;
                Job.cancel op.job "Auto-cancelling job because it is no longer needed"
              )
              (* Will release later when job finishes, if still unneeded. *)
          )
        );
      (* Ensure a build is in progress if we need one: *)
      maybe_start ~config t;
      (* Read from [t.notify] so that we re-evaluate the following when something changes. *)
      Current_incr.read (Current_incr.of_var t.notify) @@ fun () ->
      Prometheus.Counter.inc_one Metrics.evaluations_total;
      register_actions ~config ~schedule t;
      (* Return the current state: *)
      let v, update =
        match t.op with
        | `Finished x -> (x :> Op.Outcome.t Current_term.Output.t), None
        | `Retry None -> Error (`Active `Ready), None
        | `Retry (Some latched) -> (latched :> Op.Outcome.t Current_term.Output.t), Some `Ready
        | `Active (op, latched) ->
          let a =
            let started = Job.start_time op.job in
            if not (Eio.Promise.is_resolved started) then
              if Job.is_waiting_for_confirmation op.job then
                `Waiting_for_confirmation
              else
                `Ready
            else
              `Running
          in
          match latched with
          | None -> Error (`Active a), None
          | Some latched -> (latched :> Op.Outcome.t Current_term.Output.t), Some a
      in
      let metadata = { Current.Metadata.job_id = t.job_id; update } in
      Current_incr.write (v, Some metadata)
  end

  module Instances = Map.Make(String)

  (** A per-Op cache instance: the in-memory map of slots, plus the
      capabilities each slot needs. Created via {!create}; held by the
      plugin that owns this cache. *)
  type t = {
    caps : caps;
    instances : Instance.t Instances.t ref;
  }

  let create ~caps = { caps; instances = ref Instances.empty }

  (* Caller needs to notify about the change, if needed. *)
  let invalidate t key =
    let key = Op.Key.digest key in
    match Instances.find_opt key !(t.instances) with
    | Some i ->
      i.op <- `Retry None;
      Instance.invalidate i     (* (also invalidates the database) *)
    | None ->
      Db.invalidate ~op:Op.id key

  let run t ?(schedule=Schedule.default) ctx key value =
    Current_incr.of_cc begin
      Current_incr.read Current.Config.now @@ function
      | None -> Current_incr.write (Error (`Active `Ready), None)
      | Some config ->
        Log.debug (fun f -> f "set: %a" Op.pp (key, value));
        let key_digest = Op.Key.digest key in
        let value = Value.v value in
        (* Ensure the instance exists and has [i.desired = value]: *)
        let i =
          match Instances.find_opt key_digest !(t.instances) with
          | Some i ->
            (* Instance already exists in the memory cache. Update it if needed. *)
            Instance.update ~ctx i ~value;
            i
          | None ->
            (* Not in memory cache. Restore from disk if available, or create a new instance if not.
               Either way, [i.desired] is set to [value]. *)
            let release () =
              assert (Instances.mem key_digest !(t.instances));
              t.instances := Instances.remove key_digest !(t.instances);
              Prometheus.Gauge.dec_one (Metrics.memory_cache_items Op.id)
            in
            let i = Instance.load ~caps:t.caps ~release ctx key value in
            t.instances := Instances.add key_digest i !(t.instances);
            Prometheus.Gauge.inc_one (Metrics.memory_cache_items Op.id);
            i
        in
        Instance.run ~config ~schedule i
    end

  let reset t ~db =
    !(t.instances) |> Instances.iter (fun _ i -> i.Instance.ref_count <- -1);
    t.instances := Instances.empty;
    Prometheus.Gauge.set (Metrics.memory_cache_items Op.id) 0.0;
    if db then
      Db.drop_all Op.id
end

module Make(B : S.BUILDER) = struct
  module Adaptor = struct
    type t = B.t

    let id = B.id

    module Key = B.Key
    module Value = Current.Unit
    module Outcome = B.Value

    let run op job key () =
      B.build op job key

    let pp f (key, ()) = B.pp f key

    let auto_cancel = B.auto_cancel

    let latched = false
  end

  module Op = Generic(Adaptor)

  type t = Op.t
  let create = Op.create

  let get t ?schedule ctx key =
    Op.run t ?schedule ctx key ()

  let invalidate = Op.invalidate
  let reset = Op.reset
end

module Output(P : S.PUBLISHER) = struct
  module Adaptor = struct
    include P
    let run = P.publish
    let latched = false
  end

  module Op = Generic(Adaptor)

  type t = Op.t
  let create = Op.create

  let set t ?schedule ctx key value =
    Op.run t ?schedule ctx key value

  let reset = Op.reset
end

module S = S

module Db = struct
  include Db

  let key ~job_id =
    lookup_job_id job_id |> Option.map snd

  let history ~registry ~limit ~job_id =
    let key =
      match Hashtbl.find_opt registry.Current.Engine.key_of_job_id job_id with
      | None -> Db.lookup_job_id job_id
      | Some _ as k -> k
    in
    match key with
    | None -> None, []
    | Some (op, key) ->
      let complete = Db.history ~limit ~op key in
      let active = Hashtbl.find_opt registry.Current.Engine.job_id_of_key (op, key) in
      active, complete
end
