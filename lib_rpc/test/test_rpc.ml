(** Tests for the Current_rpc library *)

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

(* Create an RPC implementation using our mock *)
module Rpc = Current_rpc.Impl_with_db(Mock_current)(Mock_current.Mock_db)

(* Helper to run in-process RPC tests *)
let with_engine f =
  let engine = Mock_current.Engine.create () in
  let service = Rpc.engine engine in
  Fun.protect
    ~finally:(fun () -> Capnp_rpc.Capability.dec_ref service)
    (fun () -> f engine service)

(* ===== Type tests ===== *)

let test_stats_type () =
  let stats : Current_rpc.S.stats = {
    ok = 10;
    waiting_for_confirmation = 2;
    ready = 3;
    running = 4;
    failed = 1;
    blocked = 5;
  } in
  Alcotest.(check int) "ok" 10 stats.ok;
  Alcotest.(check int) "waiting_for_confirmation" 2 stats.waiting_for_confirmation;
  Alcotest.(check int) "ready" 3 stats.ready;
  Alcotest.(check int) "running" 4 stats.running;
  Alcotest.(check int) "failed" 1 stats.failed;
  Alcotest.(check int) "blocked" 5 stats.blocked

let test_active_type () =
  let states : Current_rpc.S.active list = [`Ready; `Running; `Waiting_for_confirmation] in
  Alcotest.(check int) "active states count" 3 (List.length states)

let test_output_type () =
  let ok : unit Current_rpc.S.output = Ok () in
  let active : unit Current_rpc.S.output = Error (`Active `Running) in
  let failed : unit Current_rpc.S.output = Error (`Msg "oops") in
  Alcotest.(check bool) "ok is Ok" true (Result.is_ok ok);
  Alcotest.(check bool) "active is Error" true (Result.is_error active);
  Alcotest.(check bool) "failed is Error" true (Result.is_error failed)

(* ===== Client type tests ===== *)

let test_query_params () =
  let params : Current_rpc.Engine.query_params = {
    op = Some "docker-build";
    ok = Some true;
    rebuild = Some false;
    job_prefix = Some "2024-01-15";
  } in
  Alcotest.(check (option string)) "op" (Some "docker-build") params.op;
  Alcotest.(check (option bool)) "ok" (Some true) params.ok;
  Alcotest.(check (option bool)) "rebuild" (Some false) params.rebuild;
  Alcotest.(check (option string)) "job_prefix" (Some "2024-01-15") params.job_prefix

let test_history_entry () =
  let entry : Current_rpc.Engine.history_entry = {
    job_id = "2024-01-15/test-job";
    build = 42L;
    outcome = Ok "success-value";
    ready = 1705312800.0;
    running = Some 1705312801.0;
    finished = 1705312900.0;
    rebuild = false;
  } in
  Alcotest.(check string) "job_id" "2024-01-15/test-job" entry.job_id;
  Alcotest.(check int64) "build" 42L entry.build;
  Alcotest.(check bool) "outcome is Ok" true (Result.is_ok entry.outcome);
  Alcotest.(check (float 0.1)) "ready" 1705312800.0 entry.ready;
  Alcotest.(check (option (float 0.1))) "running" (Some 1705312801.0) entry.running;
  Alcotest.(check (float 0.1)) "finished" 1705312900.0 entry.finished;
  Alcotest.(check bool) "rebuild" false entry.rebuild

let test_pipeline_state () =
  let success = Current_rpc.Engine.Success in
  let failed = Current_rpc.Engine.Failed "error message" in
  let active_ready = Current_rpc.Engine.Active `Ready in
  let active_running = Current_rpc.Engine.Active `Running in
  let active_waiting = Current_rpc.Engine.Active `Waiting_for_confirmation in

  (* Just check they can be constructed - pattern matching test *)
  let check_state state =
    match state with
    | Current_rpc.Engine.Success -> "success"
    | Current_rpc.Engine.Failed msg -> "failed: " ^ msg
    | Current_rpc.Engine.Active `Ready -> "active:ready"
    | Current_rpc.Engine.Active `Running -> "active:running"
    | Current_rpc.Engine.Active `Waiting_for_confirmation -> "active:waiting"
  in
  Alcotest.(check string) "success" "success" (check_state success);
  Alcotest.(check string) "failed" "failed: error message" (check_state failed);
  Alcotest.(check string) "active:ready" "active:ready" (check_state active_ready);
  Alcotest.(check string) "active:running" "active:running" (check_state active_running);
  Alcotest.(check string) "active:waiting" "active:waiting" (check_state active_waiting)

let test_confirm_level () =
  let levels = [
    Current_rpc.Engine.Harmless;
    Current_rpc.Engine.Mostly_harmless;
    Current_rpc.Engine.Average;
    Current_rpc.Engine.Above_average;
    Current_rpc.Engine.Dangerous;
  ] in
  Alcotest.(check int) "5 levels" 5 (List.length levels)

let test_rebuild_result () =
  let result : Current_rpc.Engine.rebuild_result = {
    succeeded = ["job1"; "job2"];
    failed = ["job3"];
  } in
  Alcotest.(check int) "succeeded count" 2 (List.length result.succeeded);
  Alcotest.(check int) "failed count" 1 (List.length result.failed)

(* ===== RPC Engine method tests ===== *)

let test_active_jobs () =
  with_engine (fun engine service ->
    (* Add some jobs *)
    let job1 = object
      method pp fmt = Fmt.pf fmt "test-job-1"
      method rebuild = None
    end in
    let job2 = object
      method pp fmt = Fmt.pf fmt "test-job-2"
      method rebuild = Some (fun () -> "new-job-id")
    end in
    Mock_current.Engine.add_job engine "job-1" job1;
    Mock_current.Engine.add_job engine "job-2" job2;

    match Current_rpc.Engine.active_jobs service with
    | Error _ -> Alcotest.fail "active_jobs failed"
    | Ok jobs ->
      Alcotest.(check int) "job count" 2 (List.length jobs);
      Alcotest.(check bool) "has job-1" true (List.mem "job-1" jobs);
      Alcotest.(check bool) "has job-2" true (List.mem "job-2" jobs)
  )

let test_active_jobs_empty () =
  with_engine (fun _engine service ->
    match Current_rpc.Engine.active_jobs service with
    | Error _ -> Alcotest.fail "active_jobs failed"
    | Ok jobs ->
      Alcotest.(check int) "no jobs" 0 (List.length jobs)
  )

let test_ops () =
  with_engine (fun _engine service ->
    match Current_rpc.Engine.ops service with
    | Error _ -> Alcotest.fail "ops failed"
    | Ok ops ->
      Alcotest.(check int) "ops count" 3 (List.length ops);
      Alcotest.(check bool) "has docker-build" true (List.mem "docker-build" ops);
      Alcotest.(check bool) "has git-clone" true (List.mem "git-clone" ops);
      Alcotest.(check bool) "has test-run" true (List.mem "test-run" ops)
  )

let test_pipeline_stats () =
  with_engine (fun _engine service ->
    (* Set mock stats *)
    Mock_current.Analysis.set_stats {
      ok = 10;
      waiting_for_confirmation = 2;
      ready = 3;
      running = 4;
      failed = 1;
      blocked = 5;
    };

    match Current_rpc.Engine.pipeline_stats service with
    | Error _ -> Alcotest.fail "pipeline_stats failed"
    | Ok stats ->
      Alcotest.(check int) "ok" 10 stats.ok;
      Alcotest.(check int) "waiting_for_confirmation" 2 stats.waiting_for_confirmation;
      Alcotest.(check int) "ready" 3 stats.ready;
      Alcotest.(check int) "running" 4 stats.running;
      Alcotest.(check int) "failed" 1 stats.failed;
      Alcotest.(check int) "blocked" 5 stats.blocked
  )

let test_pipeline_state_success () =
  with_engine (fun engine service ->
    Mock_current.Engine.set_value engine (Ok ());

    match Current_rpc.Engine.pipeline_state service with
    | Error _ -> Alcotest.fail "pipeline_state failed"
    | Ok state ->
      match state with
       | Current_rpc.Engine.Success -> ()
       | _ -> Alcotest.fail "Expected Success state"
  )

let test_pipeline_state_failed () =
  with_engine (fun engine service ->
    Mock_current.Engine.set_value engine (Error (`Msg "Build failed"));

    match Current_rpc.Engine.pipeline_state service with
    | Error _ -> Alcotest.fail "pipeline_state failed"
    | Ok state ->
      match state with
       | Current_rpc.Engine.Failed msg ->
         Alcotest.(check string) "error message" "Build failed" msg
       | _ -> Alcotest.fail "Expected Failed state"
  )

let test_pipeline_state_active () =
  with_engine (fun engine service ->
    Mock_current.Engine.set_value engine (Error (`Active `Running));

    match Current_rpc.Engine.pipeline_state service with
    | Error _ -> Alcotest.fail "pipeline_state failed"
    | Ok state ->
      match state with
       | Current_rpc.Engine.Active `Running -> ()
       | _ -> Alcotest.fail "Expected Active Running state"
  )

let test_pipeline_dot () =
  with_engine (fun _engine service ->
    match Current_rpc.Engine.pipeline_dot service with
    | Error _ -> Alcotest.fail "pipeline_dot failed"
    | Ok dot ->
      Alcotest.(check bool) "has digraph" true (String.length dot > 0);
      Alcotest.(check bool) "contains digraph keyword" true
        (Astring.String.is_infix ~affix:"digraph" dot)
  )

let test_get_confirm_level_none () =
  with_engine (fun engine service ->
    Mock_current.Config.set_confirm (Mock_current.Engine.config engine) None;

    match Current_rpc.Engine.get_confirm_level service with
    | Error _ -> Alcotest.fail "get_confirm_level failed"
    | Ok level ->
      Alcotest.(check (option reject)) "no level" None level
  )

let test_get_confirm_level_set () =
  with_engine (fun engine service ->
    Mock_current.Config.set_confirm
      (Mock_current.Engine.config engine)
      (Some Mock_current.Level.Dangerous);

    match Current_rpc.Engine.get_confirm_level service with
    | Error _ -> Alcotest.fail "get_confirm_level failed"
    | Ok level ->
      match level with
       | Some Current_rpc.Engine.Dangerous -> ()
       | _ -> Alcotest.fail "Expected Dangerous level"
  )

let test_set_confirm_level () =
  with_engine (fun engine service ->
    (* Set a level *)
    (match Current_rpc.Engine.set_confirm_level service (Some Current_rpc.Engine.Average) with
    | Error _ -> Alcotest.fail "set_confirm_level failed"
    | Ok () ->
      let config = Mock_current.Engine.config engine in
      (match Mock_current.Config.get_confirm config with
       | Some Mock_current.Level.Average -> ()
       | _ -> Alcotest.fail "Expected Average level to be set"));

    (* Clear the level *)
    (match Current_rpc.Engine.set_confirm_level service None with
    | Error _ -> Alcotest.fail "set_confirm_level (None) failed"
    | Ok () ->
      let config = Mock_current.Engine.config engine in
      match Mock_current.Config.get_confirm config with
       | None -> ()
       | Some _ -> Alcotest.fail "Expected level to be cleared")
  )

let test_query_empty () =
  with_engine (fun _engine service ->
    Mock_current.Mock_db.reset ();

    let params = { Current_rpc.Engine.
      op = None;
      ok = None;
      rebuild = None;
      job_prefix = None;
    } in
    match Current_rpc.Engine.query service params with
    | Error _ -> Alcotest.fail "query failed"
    | Ok entries ->
      Alcotest.(check int) "no entries" 0 (List.length entries)
  )

let test_query_with_entries () =
  with_engine (fun _engine service ->
    Mock_current.Mock_db.reset ();
    Mock_current.Mock_db.add_entry {
      job_id = "2024-01-15/job1";
      build = 1L;
      value = "ok";
      outcome = Ok "ok";  (* success *)
      ready = 1000.0;
      running = Some 1001.0;
      finished = 1002.0;
      rebuild = false;
    };
    Mock_current.Mock_db.add_entry {
      job_id = "2024-01-16/job2";
      build = 2L;
      value = "";
      outcome = Error (`Msg "failed with error");
      ready = 2000.0;
      running = None;
      finished = 2001.0;
      rebuild = true;
    };

    let params = { Current_rpc.Engine.
      op = None;
      ok = None;
      rebuild = None;
      job_prefix = None;
    } in
    match Current_rpc.Engine.query service params with
    | Error _ -> Alcotest.fail "query failed"
    | Ok entries ->
      Alcotest.(check int) "2 entries" 2 (List.length entries)
  )

let test_query_filter_by_prefix () =
  with_engine (fun _engine service ->
    Mock_current.Mock_db.reset ();
    Mock_current.Mock_db.add_entry {
      job_id = "2024-01-15/job1";
      build = 1L;
      value = "ok";
      outcome = Ok "ok";
      ready = 1000.0;
      running = Some 1001.0;
      finished = 1002.0;
      rebuild = false;
    };
    Mock_current.Mock_db.add_entry {
      job_id = "2024-01-16/job2";
      build = 2L;
      value = "";
      outcome = Error (`Msg "error");
      ready = 2000.0;
      running = None;
      finished = 2001.0;
      rebuild = true;
    };

    let params = { Current_rpc.Engine.
      op = None;
      ok = None;
      rebuild = None;
      job_prefix = Some "2024-01-15";
    } in
    match Current_rpc.Engine.query service params with
    | Error _ -> Alcotest.fail "query failed"
    | Ok entries ->
      Alcotest.(check int) "1 entry" 1 (List.length entries);
      match entries with
       | [e] -> Alcotest.(check string) "job_id" "2024-01-15/job1" e.job_id
       | _ -> Alcotest.fail "Expected exactly 1 entry"
  )

let test_rebuild_all () =
  with_engine (fun engine service ->
    (* Add jobs - some can be rebuilt, some cannot *)
    let job1 = object
      method pp fmt = Fmt.pf fmt "rebuildable-job"
      method rebuild = Some (fun () -> "new-job-1")
    end in
    let job2 = object
      method pp fmt = Fmt.pf fmt "non-rebuildable-job"
      method rebuild = None
    end in
    Mock_current.Engine.add_job engine "job-1" job1;
    Mock_current.Engine.add_job engine "job-2" job2;

    match Current_rpc.Engine.rebuild_all service ["job-1"; "job-2"; "nonexistent"] with
    | Error _ -> Alcotest.fail "rebuild_all failed"
    | Ok result ->
      Alcotest.(check int) "succeeded count" 1 (List.length result.succeeded);
      Alcotest.(check int) "failed count" 2 (List.length result.failed);
      Alcotest.(check bool) "job-1 succeeded" true (List.mem "job-1" result.succeeded);
      Alcotest.(check bool) "job-2 failed" true (List.mem "job-2" result.failed);
      Alcotest.(check bool) "nonexistent failed" true (List.mem "nonexistent" result.failed)
  )

(* ===== Job method tests ===== *)

let test_job_log () =
  with_engine (fun _engine service ->
    (* Create a mock job with a log file *)
    let log_content = "Line 1\nLine 2\nLine 3\n" in
    let _job = Mock_current.Job.create_job ~id:"test-log-job" ~log:log_content in

    let job_cap = Current_rpc.Engine.job service "test-log-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        match Current_rpc.Job.log ~start:0L job_cap with
        | Error _ -> Alcotest.fail "job log failed"
        | Ok (data, next) ->
          Alcotest.(check string) "log data" log_content data;
          Alcotest.(check int64) "next offset" (Int64.of_int (String.length log_content)) next)
  )

let test_job_log_partial () =
  with_engine (fun _engine service ->
    let log_content = "0123456789" in
    let _job = Mock_current.Job.create_job ~id:"test-partial-job" ~log:log_content in

    let job_cap = Current_rpc.Engine.job service "test-partial-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        (* Read from offset 5 *)
        match Current_rpc.Job.log ~start:5L job_cap with
        | Error _ -> Alcotest.fail "job log failed"
        | Ok (data, next) ->
          Alcotest.(check string) "partial data" "56789" data;
          Alcotest.(check int64) "next offset" 10L next)
  )

let test_job_status () =
  with_engine (fun engine service ->
    let _job = Mock_current.Job.create_job ~id:"status-job" ~log:"log" in
    let actions = object
      method pp fmt = Fmt.pf fmt "Test job description"
      method rebuild = Some (fun () -> "new-id")
    end in
    Mock_current.Engine.add_job engine "status-job" actions;

    let job_cap = Current_rpc.Engine.job service "status-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        match Current_rpc.Job.status job_cap with
        | Error _ -> Alcotest.fail "job status failed"
        | Ok status ->
          Alcotest.(check string) "id" "status-job" status.id;
          Alcotest.(check bool) "can_rebuild" true status.can_rebuild;
          Alcotest.(check bool) "can_cancel" true status.can_cancel)
  )

let test_job_cancel () =
  with_engine (fun _engine service ->
    let job = Mock_current.Job.create_job ~id:"cancel-job" ~log:"log" in

    let job_cap = Current_rpc.Engine.job service "cancel-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        match Current_rpc.Job.cancel job_cap with
        | Error _ -> Alcotest.fail "job cancel failed"
        | Ok () ->
          match job.cancelled with
           | Error (`Msg msg) ->
             Alcotest.(check bool) "cancelled" true (String.length msg > 0)
           | Ok () -> Alcotest.fail "Job should be cancelled")
  )

(* ===== Additional edge case tests ===== *)

let test_job_log_negative_offset () =
  with_engine (fun _engine service ->
    let log_content = "0123456789ABCDEF" in
    let _job = Mock_current.Job.create_job ~id:"negative-offset-job" ~log:log_content in

    let job_cap = Current_rpc.Engine.job service "negative-offset-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        (* Read from negative offset (last 5 bytes) *)
        match Current_rpc.Job.log ~start:(-5L) job_cap with
        | Error _ -> Alcotest.fail "job log failed"
        | Ok (data, _next) ->
          Alcotest.(check string) "last 5 bytes" "BCDEF" data)
  )

let test_pipeline_state_waiting () =
  with_engine (fun engine service ->
    Mock_current.Engine.set_value engine (Error (`Active `Waiting_for_confirmation));

    match Current_rpc.Engine.pipeline_state service with
    | Error _ -> Alcotest.fail "pipeline_state failed"
    | Ok state ->
      match state with
       | Current_rpc.Engine.Active `Waiting_for_confirmation -> ()
       | _ -> Alcotest.fail "Expected Active Waiting_for_confirmation state"
  )

let test_set_all_confirm_levels () =
  with_engine (fun engine service ->
    let levels = [
      Current_rpc.Engine.Harmless, Mock_current.Level.Harmless;
      Current_rpc.Engine.Mostly_harmless, Mock_current.Level.Mostly_harmless;
      Current_rpc.Engine.Average, Mock_current.Level.Average;
      Current_rpc.Engine.Above_average, Mock_current.Level.Above_average;
      Current_rpc.Engine.Dangerous, Mock_current.Level.Dangerous;
    ] in
    List.iter (fun (rpc_level, mock_level) ->
      match Current_rpc.Engine.set_confirm_level service (Some rpc_level) with
      | Error _ -> Alcotest.fail "set_confirm_level failed"
      | Ok () ->
        let config = Mock_current.Engine.config engine in
        match Mock_current.Config.get_confirm config with
         | Some l when l = mock_level -> ()
         | _ -> Alcotest.fail "Level mismatch"
    ) levels
  )

let test_query_filter_by_ok () =
  with_engine (fun _engine service ->
    Mock_current.Mock_db.reset ();
    Mock_current.Mock_db.add_entry {
      job_id = "success-job";
      build = 1L;
      value = "ok";
      outcome = Ok "ok";  (* success *)
      ready = 1000.0;
      running = Some 1001.0;
      finished = 1002.0;
      rebuild = false;
    };
    Mock_current.Mock_db.add_entry {
      job_id = "failed-job";
      build = 2L;
      value = "";
      outcome = Error (`Msg "error");  (* failure *)
      ready = 2000.0;
      running = None;
      finished = 2001.0;
      rebuild = false;
    };

    (* Query only successful *)
    let params = { Current_rpc.Engine.
      op = None;
      ok = Some true;
      rebuild = None;
      job_prefix = None;
    } in
    match Current_rpc.Engine.query service params with
    | Error _ -> Alcotest.fail "query failed"
    | Ok entries ->
      Alcotest.(check int) "1 success" 1 (List.length entries)
  )

let test_query_filter_by_rebuild () =
  with_engine (fun _engine service ->
    Mock_current.Mock_db.reset ();
    Mock_current.Mock_db.add_entry {
      job_id = "needs-rebuild";
      build = 1L;
      value = "ok";
      outcome = Ok "ok";
      ready = 1000.0;
      running = Some 1001.0;
      finished = 1002.0;
      rebuild = true;
    };
    Mock_current.Mock_db.add_entry {
      job_id = "no-rebuild";
      build = 2L;
      value = "";
      outcome = Ok "";
      ready = 2000.0;
      running = None;
      finished = 2001.0;
      rebuild = false;
    };

    (* Query only those needing rebuild *)
    let params = { Current_rpc.Engine.
      op = None;
      ok = None;
      rebuild = Some true;
      job_prefix = None;
    } in
    match Current_rpc.Engine.query service params with
    | Error _ -> Alcotest.fail "query failed"
    | Ok entries ->
      Alcotest.(check int) "1 needs rebuild" 1 (List.length entries);
      match entries with
       | [e] -> Alcotest.(check bool) "rebuild flag" true e.rebuild
       | _ -> ()
  )

let test_job_not_rebuildable () =
  with_engine (fun engine service ->
    let _job = Mock_current.Job.create_job ~id:"no-rebuild-job" ~log:"log" in
    let actions = object
      method pp fmt = Fmt.pf fmt "Non-rebuildable job"
      method rebuild = None
    end in
    Mock_current.Engine.add_job engine "no-rebuild-job" actions;

    let job_cap = Current_rpc.Engine.job service "no-rebuild-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        match Current_rpc.Job.status job_cap with
        | Error _ -> Alcotest.fail "job status failed"
        | Ok status ->
          Alcotest.(check bool) "can_rebuild" false status.can_rebuild)
  )

let test_job_approve () =
  with_engine (fun _engine service ->
    let _job = Mock_current.Job.create_job ~id:"approve-job" ~log:"log" in

    let job_cap = Current_rpc.Engine.job service "approve-job" in
    Fun.protect
      ~finally:(fun () -> Capnp_rpc.Capability.dec_ref job_cap)
      (fun () ->
        match Current_rpc.Job.approve_early_start job_cap with
        | Error _ -> Alcotest.fail "approve failed"
        | Ok () -> ())
  )

let test_history_entry_outcome_failure () =
  let entry : Current_rpc.Engine.history_entry = {
    job_id = "failed-job";
    build = 1L;
    outcome = Error "Something went wrong";
    ready = 1000.0;
    running = None;
    finished = 1001.0;
    rebuild = true;
  } in
  Alcotest.(check bool) "outcome is Error" true (Result.is_error entry.outcome);
  match entry.outcome with
   | Error msg -> Alcotest.(check string) "error message" "Something went wrong" msg
   | Ok _ -> Alcotest.fail "Expected Error"

let test_stats_zero_values () =
  let stats : Current_rpc.S.stats = {
    ok = 0;
    waiting_for_confirmation = 0;
    ready = 0;
    running = 0;
    failed = 0;
    blocked = 0;
  } in
  let total = stats.ok + stats.waiting_for_confirmation + stats.ready +
              stats.running + stats.failed + stats.blocked in
  Alcotest.(check int) "total is 0" 0 total

(* ===== Mock module tests ===== *)

let test_mock_level_conversion () =
  let levels = Mock_current.Level.values in
  Alcotest.(check int) "5 levels" 5 (List.length levels);

  (* Test round-trip *)
  List.iter (fun level ->
    let str = Mock_current.Level.to_string level in
    match Mock_current.Level.of_string str with
    | Ok level' ->
      Alcotest.(check string) "round-trip"
        (Mock_current.Level.to_string level)
        (Mock_current.Level.to_string level')
    | Error _ -> Alcotest.fail ("Failed to parse: " ^ str)
  ) levels

let test_mock_config () =
  let config = Mock_current.Config.create ~confirm:Mock_current.Level.Average () in
  (match Mock_current.Config.get_confirm config with
   | Some Mock_current.Level.Average -> ()
   | _ -> Alcotest.fail "Expected Average");

  Mock_current.Config.set_confirm config (Some Mock_current.Level.Dangerous);
  (match Mock_current.Config.get_confirm config with
   | Some Mock_current.Level.Dangerous -> ()
   | _ -> Alcotest.fail "Expected Dangerous");

  Mock_current.Config.set_confirm config None;
  (match Mock_current.Config.get_confirm config with
   | None -> ()
   | Some _ -> Alcotest.fail "Expected None")

let test_mock_engine () =
  let engine = Mock_current.Engine.create () in
  let state = Mock_current.Engine.state engine in
  Alcotest.(check bool) "initial value is Ok" true (Result.is_ok state.value);
  Alcotest.(check int) "initial jobs count" 0 (Mock_current.Job.Map.cardinal state.jobs);

  (* Add a job *)
  let job = object
    method pp fmt = Fmt.pf fmt "test"
    method rebuild = None
  end in
  Mock_current.Engine.add_job engine "test-job" job;
  let state = Mock_current.Engine.state engine in
  Alcotest.(check int) "1 job" 1 (Mock_current.Job.Map.cardinal state.jobs);

  (* Remove the job *)
  Mock_current.Engine.remove_job engine "test-job";
  let state = Mock_current.Engine.state engine in
  Alcotest.(check int) "0 jobs" 0 (Mock_current.Job.Map.cardinal state.jobs)

(* ===== Db_stub tests ===== *)

let test_db_stub () =
  let entries = Current_rpc.S.Db_stub.query () in
  Alcotest.(check int) "empty" 0 (List.length entries);

  let ops = Current_rpc.S.Db_stub.ops () in
  Alcotest.(check int) "no ops" 0 (List.length ops)

(* ===== Test suites ===== *)

let type_tests = [
  Alcotest.test_case "stats type" `Quick test_stats_type;
  Alcotest.test_case "active type" `Quick test_active_type;
  Alcotest.test_case "output type" `Quick test_output_type;
  Alcotest.test_case "query_params" `Quick test_query_params;
  Alcotest.test_case "history_entry" `Quick test_history_entry;
  Alcotest.test_case "pipeline_state" `Quick test_pipeline_state;
  Alcotest.test_case "confirm_level" `Quick test_confirm_level;
  Alcotest.test_case "rebuild_result" `Quick test_rebuild_result;
  Alcotest.test_case "history_entry_failure" `Quick test_history_entry_outcome_failure;
  Alcotest.test_case "stats_zero_values" `Quick test_stats_zero_values;
]

let engine_tests = [
  Alcotest.test_case "active_jobs" `Quick test_active_jobs;
  Alcotest.test_case "active_jobs_empty" `Quick test_active_jobs_empty;
  Alcotest.test_case "ops" `Quick test_ops;
  Alcotest.test_case "pipeline_stats" `Quick test_pipeline_stats;
  Alcotest.test_case "pipeline_state_success" `Quick test_pipeline_state_success;
  Alcotest.test_case "pipeline_state_failed" `Quick test_pipeline_state_failed;
  Alcotest.test_case "pipeline_state_active" `Quick test_pipeline_state_active;
  Alcotest.test_case "pipeline_state_waiting" `Quick test_pipeline_state_waiting;
  Alcotest.test_case "pipeline_dot" `Quick test_pipeline_dot;
  Alcotest.test_case "get_confirm_level_none" `Quick test_get_confirm_level_none;
  Alcotest.test_case "get_confirm_level_set" `Quick test_get_confirm_level_set;
  Alcotest.test_case "set_confirm_level" `Quick test_set_confirm_level;
  Alcotest.test_case "set_all_confirm_levels" `Quick test_set_all_confirm_levels;
  Alcotest.test_case "query_empty" `Quick test_query_empty;
  Alcotest.test_case "query_with_entries" `Quick test_query_with_entries;
  Alcotest.test_case "query_filter_by_prefix" `Quick test_query_filter_by_prefix;
  Alcotest.test_case "query_filter_by_ok" `Quick test_query_filter_by_ok;
  Alcotest.test_case "query_filter_by_rebuild" `Quick test_query_filter_by_rebuild;
  Alcotest.test_case "rebuild_all" `Quick test_rebuild_all;
]

let job_tests = [
  Alcotest.test_case "job_log" `Quick test_job_log;
  Alcotest.test_case "job_log_partial" `Quick test_job_log_partial;
  Alcotest.test_case "job_log_negative_offset" `Quick test_job_log_negative_offset;
  Alcotest.test_case "job_status" `Quick test_job_status;
  Alcotest.test_case "job_cancel" `Quick test_job_cancel;
  Alcotest.test_case "job_not_rebuildable" `Quick test_job_not_rebuildable;
  Alcotest.test_case "job_approve" `Quick test_job_approve;
]

let mock_tests = [
  Alcotest.test_case "mock_level_conversion" `Quick test_mock_level_conversion;
  Alcotest.test_case "mock_config" `Quick test_mock_config;
  Alcotest.test_case "mock_engine" `Quick test_mock_engine;
  Alcotest.test_case "db_stub" `Quick test_db_stub;
]

let () =
  (* Setup before tests *)
  Mock_current.Job.log_dir := "/tmp/current-rpc-test-logs";
  let _ = Bos.OS.Dir.create (Fpath.v !Mock_current.Job.log_dir) in

  Alcotest.run "current_rpc" [
    "types", type_tests;
    "engine", engine_tests;
    "job", job_tests;
    "mock", mock_tests;
  ]
