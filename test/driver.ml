open Current.Syntax

let () =
  Printexc.record_backtrace true

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let init_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

module SVar = Current.Var(struct
    type t = (unit -> unit Current.t)
    let equal = (==)
    let pp f _ = Fmt.string f "pipeline"
  end)
let selected = SVar.create ~name:"current-test" (Error (`Msg "no-test"))

module Git = Current_git_test
module Docker = Current_docker_test

let test_pipeline =
  Current.component "choose pipeline" |>
  let** make_pipeline = SVar.get selected in
  make_pipeline ()

let current_watches = ref { Current.Engine.
                            value = Error (`Active `Ready);
                            jobs = Current.Job.Map.empty }

let pp_job f j = j#pp f

let find_by_descr msg =
  let jobs = (!current_watches).Current.Engine.jobs |> Current.Job.Map.bindings in
  match List.find_opt (fun (_, job) -> Fmt.str "%t" job#pp = msg) jobs with
  | None ->
    Fmt.failwith "@[<v2>No job with description %S. We have:@,%a@]" msg
      Fmt.(Dump.list pp_job) (List.map snd jobs)
  | Some x -> x

let cancel msg =
  let job_id, _actions = find_by_descr msg in
  match Current.Job.lookup_running job_id with
  | Some job -> Current.Job.cancel job "Cancelled by user"
  | None -> Fmt.failwith "Watch %S cannot be cancelled" msg

let rebuild msg =
  let _job_id, actions = find_by_descr msg in
  match actions#rebuild with
  | None -> Fmt.failwith "Job %S cannot be rebuilt!" msg
  | Some rebuild -> rebuild () |> ignore

let stats =
  let pp f { Current_term.S.ok; waiting_for_confirmation; ready; running; failed; blocked } =
    Fmt.pf f
      "ok=%d,waiting_for_confirmation=%d,ready=%d,running=%d,failed=%d,blocked=%d"
      ok waiting_for_confirmation ready running failed blocked
  in
  Alcotest.testable pp (=)

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test env ?config ?clock ?final_stats ~name v_fn actions =
  Git.reset ();
  Docker.reset ();
  let step = ref 1 in
  let trace ~next step_result =
    if !step = 0 then raise Exit;
    begin
      Logs.info (fun f -> f "Analysis: @[%a@]" Current.Analysis.pp test_pipeline);
      let path = Fmt.str "%s.%d.dot" name !step in
      let ch = open_out path in
      let f = Format.formatter_of_out_channel ch in
      let collapse_link ~k:_ ~v:_ = None in
      let job_info { Current.Metadata.job_id = _; update } = update, None in
      let env = [] in
      Fmt.pf f "%a@!" (Current.Analysis.pp_dot ~env ~collapse_link ~job_info) test_pipeline;
      close_out ch
    end;
    current_watches := step_result;
    let { Current.Engine.value = x; _} = step_result in
    Logs.info (fun f -> f "--> %a" (Current_term.Output.pp (Fmt.any "()")) x);
    begin
      if Eio.Promise.is_resolved next then Fmt.failwith "Already ready, and nothing changed yet!";
      try actions !step with
      | Exit ->
        final_stats |> Option.iter (fun expected ->
            Alcotest.check stats "Check final stats" expected @@ Current.Analysis.quick_stat ();
          );
        SVar.set selected (Error (`Msg "test-over"));
        step := -1
    end;
    incr step;
    (* Yield until the engine has an input ready for the next iteration.
       Check [is_resolved] BEFORE giving up so we don't spuriously fail on
       the last allowed yield. *)
    let rec wait i =
      if Eio.Promise.is_resolved next then ()
      else if i = 0 then failwith "No inputs ready (tests stuck)!"
      else (
        Eio.Fiber.yield ();
        wait (i - 1)
      )
    in
    (* Worst case is the post-confirmation chain in the v2 test:
        1. publish-fork's confirm-await wakes, runs through to publish-body's yield;
        2. notify-on-start (on job_sw) wakes after start_time resolves, hits its own yield;
        3. publish-fork resumes, Switch.run job_sw waits for the child notify-on-start;
        4. notify-on-start runs [notify t] → [Engine.update ()] increments the signal counter;
        5. the bridge fiber wakes from its [Cond.await] and resolves [next].
       Plus, for tests that exercise [analyse_job] on a failing build,
       [Eio.Path.with_open_in] adds one or two more yield points before the
       cache records the failure. Seven ticks covers both cases. *)
    wait 7
  in
  try
    Eio.Switch.run (fun sw ->
      Docker.set_engine_sw sw;
      let _engine : Current.Engine.t =
        let clock = Option.map (fun c -> (c :> float Eio.Time.clock_ty Eio.Resource.t)) clock in
        Current.Engine.create ~sw ~env ?clock ?config ~trace (fun engine ->
          Docker.make_caches ~engine;
          Git.make_cache ~engine;
          (* The thunk runs once, when the engine forces it. We have
             [engine] in scope, so any cache instances the test wants to
             build (via [Current_cache.caps_of_engine engine]) can be
             constructed here. *)
          let v = v_fn engine in
          SVar.set selected (Ok v);
          Current_incr.propagate ();
          test_pipeline)
      in
      (* The engine runs as a daemon and keeps going until [trace] raises
         [Exit]; block here so the switch stays open until that happens. *)
      Eio.Fiber.await_cancel ())
  with Exit -> Docker.assert_finished ()

let test_case_gc env name fn =
  Alcotest.test_case name `Quick (fun () ->
    let old_errors = Logs.err_count () in
    (* Hold a switch open for the whole test: Driver.test opens its own
       nested switch for the engine's lifetime, but between tests, and for
       the post-test cleanup propagate, we need a live switch so that
       primitives torn down by previous pipelines can stop cleanly. *)
    Eio.Switch.run (fun _sw ->
      fn env;
      SVar.set selected (Error (`Msg "no-test"));
      Current_incr.propagate ());
    Gc.full_major ();
    Alcotest.(check int) "No errors logged" 0 @@ Logs.err_count () - old_errors;
    let data = Prometheus.CollectorRegistry.(collect default) in
    Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data
    |> String.split_on_char '\n'
    |> List.iter (fun line ->
        if Astring.String.is_prefix ~affix:"ocurrent_cache_memory_cache_items{" line then (
          match Astring.String.cut ~sep:"} " line with
          | None -> Fmt.failwith "Bad metrics line: %S" line
          | Some (key, _) when Astring.String.is_infix ~affix:"_total{" key -> ()
          | Some (key, value) ->
            if float_of_string value <> 0.0 then
              Fmt.failwith "Non-zero metric after test: %s=%s" key value
        )
      ))
