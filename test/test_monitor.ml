open Current.Syntax

let () = Driver.init_logging ()

let data = ref (Some (Ok "init"))
let data_cond = Eio.Condition.create ()
let data_mutex = Eio.Mutex.create ()
let unwatch_cond = Eio.Condition.create ()
let unwatch_mutex = Eio.Mutex.create ()

type watch = {
  set_ready : unit Eio.Promise.u;
  update : unit -> unit;
}

let w = ref None

let rec read () =
  Logs.info (fun f -> f "read");
  match !data with
  | Some x ->
    data := None;
    x
  | None ->
    Eio.Mutex.use_rw ~protect:false data_mutex (fun () ->
      Eio.Condition.await data_cond data_mutex);
    read ()

let watch update =
  Logs.info (fun f -> f "Installing watch");
  assert (!w = None);
  let ready, set_ready = Eio.Promise.create () in
  let watch = { set_ready; update } in
  w := Some watch;
  Eio.Promise.await ready;
  Logs.info (fun f -> f "Watch installed");
  fun () ->
    Logs.info (fun f -> f "Uninstalling watch");
    Eio.Mutex.use_rw ~protect:false unwatch_mutex (fun () ->
      Eio.Condition.await unwatch_cond unwatch_mutex);
    w := None;
    Logs.info (fun f -> f "Watch uninstalled")

let pp f = Fmt.string f "watch"

let monitor = Current.Monitor.create ~read ~watch ~pp

let input () =
  Current.component "input" |>
  let> () = Current.return () in
  Current.Monitor.get monitor

module Bool_var = Current.Var(struct type t = bool let pp = Fmt.bool let equal = (=) end)

let wanted = Bool_var.create ~name:"wanted" (Ok true)

let test_pipeline () =
  let* wanted = Bool_var.get wanted in
  if wanted then let* out = input () in Current.fail out
  else Current.return ()

let get_watch () =
  match !w with
  | None -> failwith "No active watch!"
  | Some w -> w

let result =
  let pp f = function
    | `Active `Waiting_for_confirmation -> Fmt.string f "Waiting for confirmation"
    | `Active `Ready -> Fmt.string f "Ready"
    | `Active `Running -> Fmt.string f "Running"
    | (`Msg m) -> Fmt.pf f "ERR: %s" m
  in
  let error = Alcotest.testable pp (=) in
  Alcotest.(result unit) error

let trace step ~next:_ { Current.Engine.value = out; _ } =
  incr step;
  let step = !step in
  match step with
  | 1 ->
    (* Although there is data ready, we shouldn't have started the read yet
       because we're still enabling the watch. *)
    Eio.Fiber.yield ();
    Alcotest.check result "Initially pending" (Error (`Active `Running)) out;
    assert (!w <> None);
    let w = get_watch () in
    Eio.Promise.resolve w.set_ready ()
  | 2 ->
    Alcotest.check result "Read complete" (Error (`Msg "init")) out;
    let w = get_watch () in
    w.update ();  (* Calls read immediately *)
    Eio.Fiber.yield ();
    w.update ();  (* Marks as out-of-date *)
    Eio.Fiber.yield ();
    w.update ();
    Eio.Fiber.yield ();
    data := Some (Ok "foo");
    Eio.Condition.broadcast data_cond  (* First read completes *)
  | 3 ->
    Alcotest.check result "Read foo" (Error (`Msg "foo")) out;
    data := Some (Ok "bar");
    Eio.Condition.broadcast data_cond  (* Second read completes *)
  | 4 ->
    Alcotest.check result "Read bar" (Error (`Msg "bar")) out;
    Bool_var.set wanted (Ok false)
  | 5 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    assert (!w <> None);
    Eio.Fiber.yield ();
    (* Wanted again, before we've finished shutting down. *)
    Bool_var.set wanted (Ok true)
  | 6 ->
    Alcotest.check result "Shutdown cancelled" (Error (`Msg "bar")) out;
    assert (!w <> None);
    Eio.Condition.broadcast unwatch_cond;   (* Allow first shutdown to finish *)
    Eio.Fiber.yield ();
    data := Some (Ok "restart");
    Eio.Condition.broadcast data_cond; (* Will re-read after shutdown *)
    Eio.Fiber.yield ();
    assert (!w <> None);
    let w = get_watch () in
    Eio.Promise.resolve w.set_ready ()
  | 7 ->
    Alcotest.check result "Read restart" (Error (`Msg "restart")) out;
    Bool_var.set wanted (Ok false);
    Eio.Fiber.yield ()
  | 8 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    Eio.Fiber.yield ();
    assert (!w <> None);
    Eio.Condition.broadcast unwatch_cond;   (* Allow shutdown to finish *)
    Eio.Fiber.yield ();
    assert (!w = None);
    Bool_var.set wanted (Ok true)
  | 9 ->
    Alcotest.check result "Pending again" (Error (`Active `Running)) out;
    Eio.Fiber.yield ();
    let w = get_watch () in
    Eio.Promise.resolve w.set_ready ();
    data := Some (Ok "baz");
    Eio.Condition.broadcast data_cond
  | 10 ->
    Alcotest.check result "Read baz" (Error (`Msg "baz")) out;
    raise Exit
  | _ ->
    assert false

let basic env =
  let step = ref 0 in
  try
    Eio.Switch.run (fun sw ->
      Current.Engine_env.init ~sw ~env;
      let _engine : Current.Engine.t =
        Current.Engine.create test_pipeline ~trace:(trace step)
      in
      Eio.Fiber.await_cancel ())
  with Exit -> ()

let tests env =
  [
    Driver.test_case_gc env "basic" basic;
  ]
