module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "core"

  let active_jobs =
    let help = "Number of ready or running job" in
    Gauge.v ~help ~namespace ~subsystem "active_jobs"
end

module Map = Map.Make(String)

(* For unit-tests: *)
let timestamp = ref Unix.gettimeofday
let sleep : (float -> unit) ref =
  ref (fun d -> Eio.Time.sleep (Engine_env.clock ()) d)

type t = {
  switch : Eio.Switch.t;
  config : Config.t;
  id : string;
  priority : Pool.priority;
  set_start_time : float Eio.Promise.u;
  start_time : float Eio.Promise.t;
  mutable path : Fpath.t option;
  log_cond : Eio.Condition.t;     (* Fires whenever log data is written, or log is closed. *)
  log_mutex : Eio.Mutex.t;
  explicit_confirm : unit Eio.Promise.t;
  set_explicit_confirm : unit Eio.Promise.u;
  mutable cancel_hooks : [ `Hooks of (string -> unit) Lwt_dllist.t | `Cancelled of string ];
  mutable waiting_for_confirmation : bool;
}

let jobs = ref Map.empty

let temp_file ~dir ~prefix ~suffix =
  let path = Filename.temp_file ~temp_dir:(Fpath.to_string dir) prefix suffix in
  Fpath.v path

let write t msg =
  match t.path with
  | None -> Log.err (fun f -> f "Job.write(%s, %S) called on closed job" t.id msg)
  | Some path ->
    let ch = open_out_gen [Open_wronly; Open_append] 0o600 (Fpath.to_string path) in
    Fun.protect ~finally:(fun () -> close_out ch) (fun () ->
      output_string ch msg;
      flush ch;
    );
    Eio.Condition.broadcast t.log_cond

let log t fmt =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    !timestamp () |> Unix.gmtime in
  let fmt = "%04d-%02d-%02d %02d:%02d.%02d: @[" ^^ fmt ^^ "@]@." in
  Fmt.kstr (write t) fmt
    (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let id t = t.id

let jobs_dir = lazy (Disk_store.state_dir "job")

let log_path job_id =
  let open Astring in
  let jobs_dir = Lazy.force jobs_dir in
  match String.cuts ~sep:"/" job_id with
  | [date; file] when
      not (String.is_prefix ~affix:"." date) &&
      not (String.is_prefix ~affix:"." file) ->
    let path = Fpath.(jobs_dir / date / (file ^ ".log")) in
    begin match Bos.OS.File.exists path with
      | Ok true -> Ok path
      | Ok false -> Fmt.error_msg "Job log %a does not exist" Fpath.pp path
      | Error _ as e -> e
    end
  | _ -> Fmt.error_msg "Invalid job ID %S" job_id

let id_of_path path =
  match Fpath.split_base path with
  | parent_dir, leaf ->
    Fpath.(base parent_dir // leaf |> segs) |> String.concat "/" |> Filename.chop_extension

let run_cancel_hooks ~reason hooks =
  let rec aux () =
    match Lwt_dllist.take_opt_l hooks with
    | None -> ()
    | Some fn -> fn reason; aux ()
  in
  aux ()

let cancel t reason =
  match t.cancel_hooks with
  | `Cancelled r2 ->
    log t "cancel(%S): already cancelled (%S)!" reason r2
  | `Hooks hooks ->
    t.cancel_hooks <- `Cancelled reason;
    log t "Cancelling: %s" reason;
    (try run_cancel_hooks ~reason hooks
     with
     | Unix.Unix_error(Unix.EPERM, "kill", _) ->
       log t "cancel(%S, %S): permission denied when killing child process (job has used sudo?)" (id t) reason
     | ex ->
       Fmt.failwith "Uncaught exception from cancel hook for %S: %a" (id t) Fmt.exn ex)

let create ?(priority=`Low) ~sw ~label ~config () =
  let jobs_dir = Lazy.force jobs_dir in
  let time = !timestamp () |> Unix.gmtime in
  let date =
    let { Unix.tm_year; tm_mon; tm_mday; _ } = time in
    Fmt.str "%04d-%02d-%02d" (tm_year + 1900) (tm_mon + 1) tm_mday
  in
  let date_dir = Fpath.(jobs_dir / date) in
  match Bos.OS.Dir.create date_dir with
  | Error (`Msg m) -> failwith m
  | Ok (_ : bool) ->
    let prefix =
      let { Unix.tm_hour; tm_min; tm_sec; _ } = time in
      Fmt.str "%02d%02d%02d-%s-" tm_hour tm_min tm_sec label
    in
    let path = temp_file ~dir:date_dir ~prefix ~suffix:".log" in
    Log.info (fun f -> f "Created new log file at@ %a" Fpath.pp path);
    let id = id_of_path path in
    let start_time, set_start_time = Eio.Promise.create () in
    let log_cond = Eio.Condition.create () in
    let log_mutex = Eio.Mutex.create () in
    let explicit_confirm, set_explicit_confirm = Eio.Promise.create () in
    let cancel_hooks = `Hooks (Lwt_dllist.create ()) in
    let t = { switch = sw; id; path = Some path; start_time; set_start_time; config; log_cond; log_mutex; cancel_hooks;
              explicit_confirm; set_explicit_confirm; waiting_for_confirmation = false; priority } in
    jobs := Map.add id t !jobs;
    Prometheus.Gauge.inc_one Metrics.active_jobs;
    Eio.Switch.on_release sw (fun () ->
        begin match t.cancel_hooks with
          | `Hooks hooks ->
            let reason = "Job complete" in
            t.cancel_hooks <- `Cancelled reason;
            run_cancel_hooks ~reason hooks
          | `Cancelled _ -> ()
        end;
        t.path <- None;
        jobs := Map.remove id !jobs;
        Prometheus.Gauge.dec_one Metrics.active_jobs;
        Eio.Condition.broadcast t.log_cond
      );
    t

let pp_id = Fmt.string

let switch t = t.switch

let is_running t = Eio.Promise.is_resolved t.start_time

let on_cancel t fn =
  match t.cancel_hooks with
  | `Cancelled reason -> fn reason
  | `Hooks hooks ->
    let (_ : _ Lwt_dllist.node) = Lwt_dllist.add_r fn hooks in
    ()

let with_handler t ~on_cancel fn =
  match t.cancel_hooks with
  | `Cancelled reason ->
    on_cancel reason;
    fn ()
  | `Hooks hooks ->
    let node = Lwt_dllist.add_r on_cancel hooks in
    Fun.protect fn ~finally:(fun () -> Lwt_dllist.remove node)

let use_pool ?(priority=`Low) ~sw t pool =
  let register_cancel cancel =
    on_cancel t (fun _ -> cancel ())
  in
  Pool.get ~priority ~sw ~register_cancel pool ()

let no_pool =
  Pool.of_fn ~label:"no pool" (fun ~priority:_ ~sw:_ -> ())

let confirm t ~pool level =
  (match t.config.Config.confirm with
   | Some threshold when Level.compare level threshold >= 0 ->
     log t "Waiting for confirm-threshold > %a" Level.pp level;
     Log.info (fun f -> f "Waiting for confirm-threshold > %a" Level.pp level);
     t.waiting_for_confirmation <- true;
     Fun.protect ~finally:(fun () -> t.waiting_for_confirmation <- false)
       (fun () ->
         Eio.Fiber.first
           (fun () ->
             Config.confirmed level t.config;
             log t "Confirm-threshold now > %a" Level.pp level;
             Log.info (fun f -> f "Confirm-threshold now > %a" Level.pp level))
           (fun () ->
             Eio.Promise.await t.explicit_confirm;
             log t "Explicit approval received for this job"))
   | _ -> ());
  let res = use_pool t ~priority:t.priority ~sw:t.switch pool in
  log t "Got resource from pool %a" Pool.pp pool;
  res

let pp_duration f d =
  let d = Duration.to_f d in
  if d > 120.0 then Fmt.pf f "%.1f minutes" (d /. 60.)
  else if d > 2.0 then Fmt.pf f "%.1f seconds" d
  else Fmt.pf f "%f seconds" d

let start_with ?timeout ~pool ~level t =
  let r = confirm t ~pool level in
  if is_running t then (
    Log.warn (fun f -> f "start called, but job %s is already running!" t.id);
    Fmt.failwith "Job.start called twice!"
  );
  Eio.Promise.resolve t.set_start_time (!timestamp ());
  timeout |> Option.iter (fun duration ->
    (* Fork as a *daemon* on the job's switch: a non-daemon fiber would
       keep [Switch.run] alive until the sleep finished, blocking the
       job's on_release hook (and the log-stream broadcast) for every
       job that completes before its timeout. *)
    Eio.Fiber.fork_daemon ~sw:t.switch (fun () ->
      !sleep (Duration.to_f duration);
      (match t.cancel_hooks with
       | `Cancelled _ -> ()
       | `Hooks _ -> cancel t (Fmt.str "Timeout (%a)" pp_duration duration));
      `Stop_daemon
    )
  );
  r

let start ?timeout ?(pool=no_pool) = start_with ?timeout ~pool

let start_time t = t.start_time

let wait_for_log_data t =
  (* [use_ro] so that if the awaiting fiber is cancelled (e.g. the HTTP
     client serving a log-tail disconnects) the mutex is simply released —
     not poisoned, which would break every subsequent request for the same
     job's log. *)
  Eio.Mutex.use_ro t.log_mutex (fun () ->
    Eio.Condition.await t.log_cond t.log_mutex)

let lookup_running id = Map.find_opt id !jobs

let is_waiting_for_confirmation t = t.waiting_for_confirmation

let approve_early_start t =
  if not (Eio.Promise.is_resolved t.explicit_confirm) then
    Eio.Promise.resolve t.set_explicit_confirm ()

let cancelled_state t =
  match t.cancel_hooks with
  | `Cancelled reason -> Error (`Msg reason)
  | `Hooks _ -> Ok ()

let jobs () = !jobs
