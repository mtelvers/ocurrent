(* Unified RPC client implementation.

   This module provides:
   - All client operations (pipeline overview, job control, configuration)
   - Cmdliner integration for embedding in applications
   - Standalone client command
*)

open Capnp_rpc

(* ===== Helper Functions ===== *)

let pp_timestamp ppf ts =
  let open Unix in
  let tm = localtime ts in
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let level_to_string = function
  | Engine.Harmless -> "harmless"
  | Engine.Mostly_harmless -> "mostly-harmless"
  | Engine.Average -> "average"
  | Engine.Above_average -> "above-average"
  | Engine.Dangerous -> "dangerous"

let string_to_level = function
  | "harmless" -> Some Engine.Harmless
  | "mostly-harmless" -> Some Engine.Mostly_harmless
  | "average" -> Some Engine.Average
  | "above-average" -> Some Engine.Above_average
  | "dangerous" -> Some Engine.Dangerous
  | "none" | "disabled" -> None
  | s -> Fmt.failwith "Unknown confirmation level: %S" s

(* ===== Client Operations ===== *)

module Ops = struct
  (* Pipeline overview *)
  let overview engine =
    match Engine.pipeline_stats engine with
    | Error `Capnp e ->
      Fmt.epr "Failed to get stats: %a@." Capnp_rpc.Error.pp e;
      Error (`Capnp e)
    | Ok stats ->
      Fmt.pr "@[<v>Pipeline Statistics:@,\
              @,\
              OK:                       %d@,\
              Waiting for confirmation: %d@,\
              Ready:                    %d@,\
              Running:                  %d@,\
              Failed:                   %d@,\
              Blocked:                  %d@,\
              @,\
              Total stages: %d@]@.@."
        stats.ok
        stats.waiting_for_confirmation
        stats.ready
        stats.running
        stats.failed
        stats.blocked
        (stats.ok + stats.waiting_for_confirmation + stats.ready +
         stats.running + stats.failed + stats.blocked);
      match Engine.pipeline_state engine with
      | Error `Capnp e ->
        Fmt.epr "Failed to get state: %a@." Capnp_rpc.Error.pp e;
        Error (`Capnp e)
      | Ok state ->
        let state_str = match state with
          | Engine.Success -> "SUCCESS"
          | Engine.Failed msg -> Fmt.str "FAILED: %s" msg
          | Engine.Active `Ready -> "ACTIVE (ready to run)"
          | Engine.Active `Running -> "ACTIVE (running)"
          | Engine.Active `Waiting_for_confirmation -> "ACTIVE (waiting for confirmation)"
        in
        Fmt.pr "Pipeline State: %s@." state_str;
        Ok ()

  (* List active jobs *)
  let jobs engine =
    match Engine.active_jobs engine with
    | Error `Capnp e ->
      Fmt.epr "Failed to list jobs: %a@." Capnp_rpc.Error.pp e;
      Error (`Capnp e)
    | Ok jobs ->
      if jobs = [] then
        Fmt.pr "No active jobs.@."
      else begin
        Fmt.pr "@[<v>Active Jobs (%d):@," (List.length jobs);
        List.iter (fun j -> Fmt.pr "  %s@," j) jobs;
        Fmt.pr "@]@."
      end;
      Ok ()

  (* Show job status *)
  let status engine job_id =
    let job = Engine.job engine job_id in
    Fun.protect
      ~finally:(fun () -> Capability.dec_ref job)
      (fun () ->
        match Job.status job with
        | Error `Capnp e ->
          Fmt.epr "Failed to get status: %a@." Capnp_rpc.Error.pp e;
          Error (`Capnp e)
        | Ok { Job.id; description; can_cancel; can_rebuild } ->
          Fmt.pr "@[<v2>Job %S:@,\
                  Description: @[%a@]@,\
                  Can cancel: %b@,\
                  Can rebuild: %b@]@."
            id Fmt.lines description can_cancel can_rebuild;
          Ok ())

  (* Show job log *)
  let log engine job_id =
    let job = Engine.job engine job_id in
    let rec stream start =
      match Job.log ~start job with
      | Error `Capnp e ->
        Fmt.epr "Failed to get log: %a@." Capnp_rpc.Error.pp e;
        Error (`Capnp e)
      | Ok (data, next) ->
        if data = "" then Ok ()
        else begin
          output_string stdout data;
          flush stdout;
          stream next
        end
    in
    Fun.protect
      ~finally:(fun () -> Capability.dec_ref job)
      (fun () -> stream 0L)

  (* Cancel a job *)
  let cancel engine job_id =
    let job = Engine.job engine job_id in
    Fun.protect
      ~finally:(fun () -> Capability.dec_ref job)
      (fun () ->
        match Job.cancel job with
        | Error `Capnp e ->
          Fmt.epr "Failed to cancel: %a@." Capnp_rpc.Error.pp e;
          Error (`Capnp e)
        | Ok () ->
          Fmt.pr "Cancelled.@.";
          Ok ())

  (* Rebuild a single job *)
  let rebuild engine job_id =
    let job = Engine.job engine job_id in
    Fmt.pr "Requesting rebuild...@.";
    let new_job = Job.rebuild job in
    let rec stream start =
      match Job.log ~start new_job with
      | Error `Capnp e ->
        Fmt.epr "Failed to get log: %a@." Capnp_rpc.Error.pp e;
        Error (`Capnp e)
      | Ok (data, next) ->
        if data = "" then Ok ()
        else begin
          output_string stdout data;
          flush stdout;
          stream next
        end
    in
    Fun.protect
      ~finally:(fun () ->
         Capability.dec_ref job;
         Capability.dec_ref new_job)
      (fun () -> stream 0L)

  (* Approve early start *)
  let start engine job_id =
    let job = Engine.job engine job_id in
    Fun.protect
      ~finally:(fun () -> Capability.dec_ref job)
      (fun () ->
        match Job.approve_early_start job with
        | Error `Capnp e ->
          Fmt.epr "Failed to approve: %a@." Capnp_rpc.Error.pp e;
          Error (`Capnp e)
        | Ok () ->
          Fmt.pr "Job approved to start.@.";
          Ok ())

  (* Query job history *)
  let query engine ~op ~ok ~rebuild ~job_prefix =
    let params = { Engine.op; ok; rebuild; job_prefix } in
    match Engine.query engine params with
    | Error `Capnp e ->
      Fmt.epr "Query failed: %a@." Capnp_rpc.Error.pp e;
      Error (`Capnp e)
    | Ok entries ->
      if entries = [] then
        Fmt.pr "No matching jobs found.@."
      else begin
        Fmt.pr "@[<v>Job History (%d entries):@,@," (List.length entries);
        entries |> List.iter (fun (entry : Engine.history_entry) ->
          let outcome_str = match entry.outcome with
            | Ok v when v = "" -> "OK"
            | Ok v -> Fmt.str "OK: %s" v
            | Error e -> Fmt.str "FAILED: %s" e
          in
          let running_str = match entry.running with
            | Some t -> Fmt.str "  Started:  %a@," pp_timestamp t
            | None -> ""
          in
          Fmt.pr "@[<v>Job: %s (build #%Ld)@,\
                  Outcome: %s@,\
                  Ready:    %a@,\
                  %s\
                  Finished: %a@,\
                  Rebuild requested: %b@]@,@,"
            entry.job_id entry.build
            outcome_str
            pp_timestamp entry.ready
            running_str
            pp_timestamp entry.finished
            entry.rebuild
        );
        Fmt.pr "@]@."
      end;
      Ok ()

  (* List operation types *)
  let ops engine =
    match Engine.ops engine with
    | Error `Capnp e ->
      Fmt.epr "Failed to list ops: %a@." Capnp_rpc.Error.pp e;
      Error (`Capnp e)
    | Ok ops ->
      if ops = [] then
        Fmt.pr "No operation types found.@."
      else begin
        Fmt.pr "@[<v>Operation Types:@,";
        List.iter (fun op -> Fmt.pr "  %s@," op) ops;
        Fmt.pr "@]@."
      end;
      Ok ()

  (* Get pipeline DOT graph *)
  let dot engine =
    match Engine.pipeline_dot engine with
    | Error `Capnp e ->
      Fmt.epr "Failed to get DOT: %a@." Capnp_rpc.Error.pp e;
      Error (`Capnp e)
    | Ok dot ->
      print_string dot;
      Ok ()

  (* Get/set confirmation level *)
  let confirm engine set_level =
    match set_level with
    | Some level_str ->
      let level = string_to_level level_str in
      (match Engine.set_confirm_level engine level with
      | Error `Capnp e ->
        Fmt.epr "Failed to set level: %a@." Capnp_rpc.Error.pp e;
        Error (`Capnp e)
      | Ok () ->
        (match level with
         | None -> Fmt.pr "Confirmation disabled.@."
         | Some l -> Fmt.pr "Confirmation level set to: %s@." (level_to_string l));
        Ok ())
    | None ->
      match Engine.get_confirm_level engine with
      | Error `Capnp e ->
        Fmt.epr "Failed to get level: %a@." Capnp_rpc.Error.pp e;
        Error (`Capnp e)
      | Ok level ->
        (match level with
         | None -> Fmt.pr "Confirmation: disabled@."
         | Some l -> Fmt.pr "Confirmation level: %s@." (level_to_string l));
        Ok ()

  (* Bulk rebuild *)
  let rebuild_all engine job_ids =
    if job_ids = [] then begin
      Fmt.pr "No job IDs specified.@.";
      Ok ()
    end else begin
      Fmt.pr "Requesting rebuild of %d jobs...@." (List.length job_ids);
      match Engine.rebuild_all engine job_ids with
      | Error `Capnp e ->
        Fmt.epr "Rebuild failed: %a@." Capnp_rpc.Error.pp e;
        Error (`Capnp e)
      | Ok result ->
        if result.succeeded <> [] then begin
          Fmt.pr "@[<v>Successfully queued for rebuild:@,";
          List.iter (fun id -> Fmt.pr "  %s@," id) result.succeeded;
          Fmt.pr "@]"
        end;
        if result.failed <> [] then begin
          Fmt.pr "@[<v>Failed to rebuild:@,";
          List.iter (fun id -> Fmt.pr "  %s@," id) result.failed;
          Fmt.pr "@]"
        end;
        Fmt.pr "@.";
        Ok ()
    end
end

(* ===== Connection Handling ===== *)

let connect ~sw ~net cap_uri =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let sr = Capnp_rpc_unix.Vat.import_exn vat cap_uri in
  Sturdy_ref.connect_exn sr

let with_engine ~net cap_uri f =
  Eio.Switch.run @@ fun sw ->
  let engine = connect ~sw ~net cap_uri in
  Fun.protect
    ~finally:(fun () -> Capability.dec_ref engine)
    (fun () -> f engine)

(* ===== Cmdliner Integration ===== *)

module Cmdliner = struct
  open Cmdliner

  (* Run an operation inside Eio_main.run, with a net cap. [with_engine]
     opens its own switch for the connection's lifetime. *)
  let run_op cap_uri f =
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    match with_engine ~net cap_uri f with
    | Ok () -> `Ok ()
    | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)

  (* Build subcommands parameterized by cap_uri term *)
  let make_subcommands cap_uri =
    (* Subcommand: overview *)
    let overview_cmd =
      let doc = "Show pipeline statistics and state" in
      let run cap_uri = run_op cap_uri Ops.overview in
      Cmd.v (Cmd.info "overview" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: jobs *)
    let jobs_cmd =
      let doc = "List active jobs" in
      let run cap_uri = run_op cap_uri Ops.jobs in
      Cmd.v (Cmd.info "jobs" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: status *)
    let status_cmd =
      let doc = "Show status of a specific job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id = run_op cap_uri (fun e -> Ops.status e job_id) in
      Cmd.v (Cmd.info "status" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: log *)
    let log_cmd =
      let doc = "Show log of a specific job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id = run_op cap_uri (fun e -> Ops.log e job_id) in
      Cmd.v (Cmd.info "log" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: cancel *)
    let cancel_cmd =
      let doc = "Cancel a running job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id = run_op cap_uri (fun e -> Ops.cancel e job_id) in
      Cmd.v (Cmd.info "cancel" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: rebuild *)
    let rebuild_cmd =
      let doc = "Rebuild a job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id = run_op cap_uri (fun e -> Ops.rebuild e job_id) in
      Cmd.v (Cmd.info "rebuild" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: start *)
    let start_cmd =
      let doc = "Approve early start for a job waiting for confirmation" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id = run_op cap_uri (fun e -> Ops.start e job_id) in
      Cmd.v (Cmd.info "start" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: query *)
    let query_cmd =
      let doc = "Query job history" in
      let op =
        Arg.value @@
        Arg.opt Arg.(some string) None @@
        Arg.info ["op"] ~doc:"Filter by operation type" ~docv:"OP"
      in
      let ok =
        Arg.value @@
        Arg.opt Arg.(some bool) None @@
        Arg.info ["ok"] ~doc:"Filter by success (true) or failure (false)"
      in
      let rebuild =
        Arg.value @@
        Arg.opt Arg.(some bool) None @@
        Arg.info ["rebuild"] ~doc:"Filter by rebuild-needed flag"
      in
      let prefix =
        Arg.value @@
        Arg.opt Arg.(some string) None @@
        Arg.info ["prefix"] ~doc:"Filter by job ID prefix (e.g., date)" ~docv:"PREFIX"
      in
      let run cap_uri op ok rebuild job_prefix =
        run_op cap_uri (fun e -> Ops.query e ~op ~ok ~rebuild ~job_prefix)
      in
      Cmd.v (Cmd.info "query" ~doc) Term.(ret (const run $ cap_uri $ op $ ok $ rebuild $ prefix))
    in

    (* Subcommand: ops *)
    let ops_cmd =
      let doc = "List operation types" in
      let run cap_uri = run_op cap_uri Ops.ops in
      Cmd.v (Cmd.info "ops" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: dot *)
    let dot_cmd =
      let doc = "Output pipeline as DOT graph (pipe to 'dot -Tsvg' for visualization)" in
      let run cap_uri = run_op cap_uri Ops.dot in
      Cmd.v (Cmd.info "dot" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: confirm *)
    let confirm_cmd =
      let doc = "Get or set the confirmation threshold" in
      let set_level =
        Arg.value @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"Level to set (harmless/mostly-harmless/average/above-average/dangerous/none). Omit to query."
          ~docv:"LEVEL"
      in
      let run cap_uri set_level =
        run_op cap_uri (fun e -> Ops.confirm e set_level)
      in
      Cmd.v (Cmd.info "confirm" ~doc) Term.(ret (const run $ cap_uri $ set_level))
    in

    (* Subcommand: rebuild-all *)
    let rebuild_all_cmd =
      let doc = "Rebuild multiple jobs" in
      let job_ids =
        Arg.non_empty @@
        Arg.pos_all Arg.string [] @@
        Arg.info [] ~doc:"Job IDs to rebuild" ~docv:"JOB_IDS"
      in
      let run cap_uri job_ids =
        run_op cap_uri (fun e -> Ops.rebuild_all e job_ids)
      in
      Cmd.v (Cmd.info "rebuild-all" ~doc) Term.(ret (const run $ cap_uri $ job_ids))
    in

    [overview_cmd; jobs_cmd; status_cmd; log_cmd; cancel_cmd; rebuild_cmd;
     start_cmd; query_cmd; ops_cmd; dot_cmd; confirm_cmd; rebuild_all_cmd]

  let cap_uri =
    let doc = "Sturdy URI for the OCurrent engine" in
    Arg.required @@
    Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
    Arg.info ["cap"] ~docv:"CAP" ~doc

  let cmd name version =
    let doc = "Client for an OCurrent RPC engine" in
    let info = Cmd.info name ~version ~doc in
    Cmd.group info (make_subcommands cap_uri)
end
