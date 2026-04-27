module Job = Current.Job

let read path =
  let ch = open_in_bin (Fpath.to_string path) in
  let data = really_input_string ch (in_channel_length ch) in
  close_in ch;
  data

let ( >>!= ) x f =
  match x with
  | Ok y -> f y
  | Error `Msg m -> failwith m

(* Each test takes [env] and capture the capabilities locally so [Job.create]
   gets the same clock/process_mgr/fs the engine would in production. *)
let create_job ~env ?priority ~sw ~label ~config () =
  Job.create ?priority ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    ~fs:(Eio.Stdenv.fs env)
    ~label ~config ()

let streams env =
  Job.timestamp := (fun () -> 0.0);
  Eio.Switch.run @@ fun sw ->
  let config = Current.Config.v () in
  let job = create_job ~env ~sw ~label:"streams" ~config () in
  let cmd = ["sh"; "-c"; "echo out1; echo >&2 out2; echo out3"] in
  Current.Process.exec ~cancellable:true ~job cmd >>!= fun () ->
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Combined results" "1970-01-01 00:00.00: Exec: \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\"\n\
                                              out1\nout2\nout3\n" (read path)

let output env =
  Job.timestamp := (fun () -> 0.0);
  Eio.Switch.run @@ fun sw ->
  let config = Current.Config.v () in
  let job = create_job ~env ~sw ~label:"output" ~config () in
  let cmd = ["sh"; "-c"; "echo out1; echo >&2 out2; echo out3"] in
  Current.Process.check_output ~cancellable:true ~job cmd >>!= fun out ->
  Alcotest.(check string) "Output" "out1\nout3\n" out;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\"\n\
                                 out2\n" (read path)

let pp_cmd ppf args =
  let remove_token s =
    match Astring.String.cut ~sep:":" s with
    | Some ("token", _secret) -> "token:<TOKEN>"
    | _ -> s
  in
  Current.Process.pp_cmd ppf (List.map remove_token args)

let pp_command env =
  Job.timestamp := (fun () -> 0.0);
  Eio.Switch.run @@ fun sw ->
  let config = Current.Config.v () in
  let job = create_job ~env ~sw ~label:"output" ~config () in
  let cmd = ["echo"; "token:abcdefgh"] in
  Current.Process.check_output ~pp_cmd ~cancellable:true ~job cmd >>!= fun out ->
  Alcotest.(check string) "Output" "token:abcdefgh\n" out;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: \"echo\" \"token:<TOKEN>\"\n" (read path)

let cancel env =
  Job.timestamp := (fun () -> 0.0);
  Eio.Switch.run @@ fun sw ->
  let config = Current.Config.v () in
  let job = create_job ~env ~sw ~label:"output" ~config () in
  let cmd = ["sleep"; "120"] in
  let res =
    Eio.Fiber.first
      (fun () ->
        Eio.Fiber.yield ();
        Current.Job.cancel job "Timeout";
        Eio.Fiber.await_cancel ())
      (fun () -> Current.Process.exec ~cancellable:true ~job cmd)
  in
  begin match res with
    | Ok () -> Alcotest.fail "Should have failed!"
    | Error `Msg m when Astring.String.is_prefix ~affix:"Command \"sleep\" \"120\" failed with signal" m -> ()
    | Error `Msg m -> Alcotest.failf "Expected signal error, not %S" m
  end;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: \"sleep\" \"120\"\n\
                                 1970-01-01 00:00.00: Cancelling: Timeout\n" (read path)

(* For checking pool semantics we need to observe whether [Job.start] has
   unblocked. Fork it onto a switch and track the outcome in a [Promise]. *)
type start_state =
  | Pending
  | Returned
  | Failed of exn

let pp_start_state f = function
  | Pending -> Fmt.string f "Pending"
  | Returned -> Fmt.string f "Returned"
  | Failed ex -> Fmt.exn f ex

let start_state_t = Alcotest.testable pp_start_state (=)

let observe p =
  if Eio.Promise.is_resolved p then Eio.Promise.await p
  else Pending

let fork_start ~sw ?pool ~level job =
  let p, u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
    match Job.start ?pool ~level job with
    | () -> Eio.Promise.resolve u Returned
    | exception ex -> Eio.Promise.resolve u (Failed ex));
  p

let pool env =
  Eio.Switch.run @@ fun outer_sw ->
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  (* sw2 is outer (so sw1's release fires first). Fork order must still
     be job1 then job2 so job1 wins the pool slot. *)
  Eio.Switch.run @@ fun sw2 ->
  let job2 = create_job ~env ~sw:sw2 ~label:"job-2" ~config () in
  (Eio.Switch.run @@ fun sw1 ->
   let job1 = create_job ~env ~sw:sw1 ~label:"job-1" ~config () in
   let s1 = fork_start ~sw:outer_sw ~pool ~level:Current.Level.Harmless job1 in
   let s2 = fork_start ~sw:outer_sw ~pool ~level:Current.Level.Harmless job2 in
   Eio.Fiber.yield ();
   Alcotest.check start_state_t "First job started" Returned (observe s1);
   Alcotest.check start_state_t "Second job queued" Pending (observe s2);
   (* sw1 releases next — we also need to observe s2 *after* that, so
      smuggle s2 back out. *)
   s2) |> fun s2 ->
  Eio.Fiber.yield ();
  Alcotest.check start_state_t "Second job ready" Returned (observe s2)

let pool_cancel env =
  Eio.Switch.run @@ fun outer_sw ->
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 0 in
  Eio.Switch.run @@ fun sw1 ->
  let job1 = create_job ~env ~sw:sw1 ~label:"job-1" ~config () in
  let s1 = fork_start ~sw:outer_sw ~pool ~level:Current.Level.Harmless job1 in
  Alcotest.check start_state_t "Job queued" Pending (observe s1);
  Current.Job.cancel job1 "Cancel";
  Eio.Fiber.yield ();
  Job.log job1 "Continuing job for a bit";
  Alcotest.check start_state_t "Job cancelled"
    (Failed (Failure "Cancelled waiting for resource from pool \"test\""))
    (observe s1)

let pool_priority env =
  Eio.Switch.run @@ fun outer_sw ->
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  (* Nest so sw1 closes first (releasing the slot), sw3 second (tested
     by s3 becoming Returned), sw2 last. Fork order: job1, job2, job3. *)
  Eio.Switch.run @@ fun sw2 ->
  let job2 = create_job ~env ~sw:sw2 ~label:"job-2" ~config () in
  let (s2, s3) =
    Eio.Switch.run @@ fun sw3 ->
    let job3 = create_job ~env ~priority:`High ~sw:sw3 ~label:"job-3" ~config () in
    let s2, s3 =
      Eio.Switch.run @@ fun sw1 ->
      let job1 = create_job ~env ~sw:sw1 ~label:"job-1" ~config () in
      let s1 = fork_start ~sw:outer_sw ~pool ~level:Current.Level.Harmless job1 in
      let s2 = fork_start ~sw:outer_sw ~pool ~level:Current.Level.Harmless job2 in
      let s3 = fork_start ~sw:outer_sw ~pool ~level:Current.Level.Harmless job3 in
      Eio.Fiber.yield ();
      Alcotest.check start_state_t "First job started" Returned (observe s1);
      Alcotest.check start_state_t "Second job queued" Pending (observe s2);
      Alcotest.check start_state_t "Third job queued" Pending (observe s3);
      s2, s3
    in
    (* sw1 released *)
    Eio.Fiber.yield ();
    Alcotest.check start_state_t "Second job queued" Pending (observe s2);
    Alcotest.check start_state_t "High-priority third job ready" Returned (observe s3);
    s2, s3
  in
  (* sw3 released *)
  let _ = s3 in
  Eio.Fiber.yield ();
  Alcotest.check start_state_t "Second job ready" Returned (observe s2)

let tests env =
  [
    Driver.test_case_gc env "streams" streams;
    Driver.test_case_gc env "output" output;
    Driver.test_case_gc env "pp_cmd" pp_command;
    Driver.test_case_gc env "cancel" cancel;
    Driver.test_case_gc env "pool" pool;
    Driver.test_case_gc env "pool_cancel" pool_cancel;
    Driver.test_case_gc env "pool_priority" pool_priority;
  ]
