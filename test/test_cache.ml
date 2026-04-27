open Current.Syntax

let () = Driver.init_logging ()

module Builds = Map.Make(String)

module Build = struct
  module Key = Current.String
  module Value = Current.String

  type waker = string Current.or_error Eio.Promise.u

  type t = waker Builds.t ref

  let create () = ref Builds.empty

  let id = "test-build"

  let pp = Fmt.string

  let build t job key =
    Current.Job.start job ~level:Current.Level.Average;
    if Builds.mem key !t then Fmt.failwith "Already building %s!" key;
    let finished, set_finished = Eio.Promise.create () in
    t := Builds.add key set_finished !t;
    Eio.Promise.await finished

  let auto_cancel = true
end

module BC = Current_cache.Make(Build)

let get bc ?schedule builds x =
  Current.component "get %s" x |>
  let> () = Current.return () in
  BC.get bc ?schedule builds x

let pp_error f (`Msg m) = Fmt.string f m

let disk_cache () =
  Current_cache.Db.query ~op:"test-build" ~rebuild:false ()
  |> List.map (fun { Current_cache.Db.job_id = _; outcome; ready; running; finished; build; value = _; rebuild = _ } ->
      let running = Option.map truncate running in
      Fmt.str "%a %.0f/%a/%.0f +%Ld" Fmt.(result ~ok:string ~error:pp_error) outcome
        ready Fmt.(option ~none:(any "-") int) running finished build
    )
  |> List.sort compare

let database = Alcotest.(list string)

(* All cache tests share the [test-build] / [publish] / [latched] tables so
   we drop them at the start of each test. The mock clock is a fresh one
   per test, passed to {!Engine.create} via [?clock]. *)
let mock_clock () =
  let c = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time c 0.0;
  c

let basic env =
  let result = ref "none" in
  let bc_ref = ref None in
  let pipeline ~bc builds () =
    let+ x = get bc builds "a" in
    result := x
  in
  Current_cache.Db.drop_all "test-build";
  let clock = mock_clock () in
  Alcotest.check database "Disk store initially empty" [] @@ disk_cache ();
  let builds = Build.create () in
  Driver.test env ~name:"cache" ~clock (fun engine ->
    let bc = BC.create ~caps:(Current_cache.caps_of_engine engine) in
    bc_ref := Some bc;
    pipeline ~bc builds
  ) @@ function
  | 1 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Eio_mock.Clock.set_time clock 1.0;
    Eio.Promise.resolve b @@ Ok "done"
  | 2 ->
    Alcotest.(check string) "Result correct" "done" !result;
    Alcotest.check database "Result stored" ["done 0/0/1 +0"] @@ disk_cache ();
    Driver.rebuild "a (completed)";
  | 3 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Eio.Promise.resolve b @@ Ok "rebuild"
  | 4 ->
    Alcotest.(check string) "Rebuild result" "rebuild" !result;
    raise Exit
  | _ ->
    assert false

let result_t =
  Alcotest.testable
    (Current_term.Output.pp Fmt.string)
    (Current_term.Output.equal (=))

let expires env =
  let result = ref (Error (`Msg ("uninitialised"))) in
  let five_s = Current_cache.Schedule.v ~valid_for:(Duration.of_sec 5) () in
  let ten_s = Current_cache.Schedule.v ~valid_for:(Duration.of_sec 10) () in
  let pipeline ~bc builds () =
    Current.state (
      let+ x = get bc ~schedule:ten_s builds "a"
      and+ y = get bc ~schedule:five_s builds "a"
      in
      Fmt.str "%s,%s" x y
    )
    |> Current.map (fun x -> result := x)
  in
  Current_cache.Db.drop_all "test-build";
  let clock = mock_clock () in
  Alcotest.check database "Disk store initially empty" [] @@ disk_cache ();
  let builds = Build.create () in
  Driver.test env ~name:"cache" ~clock (fun engine ->
    let bc = BC.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~bc builds
  ) @@ function
  | 1 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Eio_mock.Clock.set_time clock 1.0;
    Eio.Promise.resolve b @@ Ok "done"
  | 2 ->
    Alcotest.check database "Result stored" ["done 0/0/1 +0"] @@ disk_cache ();
    Alcotest.(check result_t) "Result correct" (Ok "done,done") !result;
    Eio_mock.Clock.set_time clock 7.0
  | 3 ->
    Alcotest.(check result_t) "Result latched" (Ok "done,done") !result;
    let b = Builds.find "a" !builds in
    Eio_mock.Clock.set_time clock 8.0;
    Alcotest.check database "Disk store not invalidated" ["done 0/0/1 +0"] @@ disk_cache ();
    Eio.Promise.resolve b @@ Ok "rebuild"
  | _ ->
    Alcotest.check database "Result stored" ["done 0/0/1 +0"; "rebuild 7/7/8 +1"] @@ disk_cache ();
    Alcotest.(check result_t) "Result correct" (Ok "rebuild,rebuild") !result;
    raise Exit

module Bool_var = Current.Var(struct type t = bool let pp = Fmt.bool let equal = (=) end)
let wanted = Bool_var.create ~name:"wanted" (Ok true)

let autocancel env =
  let result = ref "none" in
  let builds = Build.create () in
  let pipeline ~bc () =
    let* wanted = Bool_var.get wanted in
    let+ r =
      if wanted then get bc builds "a"
      else Current.return "unwanted"
    in
    result := r
  in
  Current_cache.Db.drop_all "test-build";
  let clock = mock_clock () in
  Alcotest.check database "Disk store initially empty" [] @@ disk_cache ();
  Driver.test env ~name:"cache" ~clock (fun engine ->
    let bc = BC.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~bc
  ) @@ function
  | 1 ->
    Alcotest.(check string) "Initially pending" "none" !result;
    Bool_var.set wanted @@ Ok false
  | 2 ->
    Alcotest.(check string) "Not wanted" "unwanted" !result;
    Bool_var.set wanted @@ Ok true
  | 3 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Eio_mock.Clock.set_time clock 1.0;
    Eio.Promise.resolve b @@ Ok "old-build"
  | 4 ->
    Alcotest.(check string) "No update yet" "unwanted" !result;
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Eio_mock.Clock.set_time clock 2.0;
    Eio.Promise.resolve b @@ Ok "new-build"
  | 5 ->
    Alcotest.(check string) "Rebuild done" "new-build" !result;
    raise Exit
  | _ ->
    assert false

module Publish = struct
  module Key = Current.String
  module Value = Current.String
  module Outcome = Current.Unit

  type t = {
    mutable state : string;
    mutable next : string;
    mutable set_finished : unit Current.or_error Eio.Promise.u option;
  }

  let id = "publish"

  let complete t v =
    match t.set_finished with
    | None -> failwith "Publish.complete: nothing in progress!"
    | Some set_finished ->
      t.set_finished <- None;
      if v = Ok () then t.state <- t.next;
      t.next <- "unset";
      Eio.Promise.resolve set_finished v

  let publish t job key value =
    Logs.info (fun f -> f "test_cache.publish");
    assert (key = "foo");
    assert (t.set_finished = None);
    Current.Job.start job ~level:Current.Level.Average;
    let finished, set_finished = Eio.Promise.create () in
    t.set_finished <- Some set_finished;
    t.state <- t.state ^ "-changing";
    t.next <- value;
    Current.Job.with_handler job
      (fun () -> Eio.Promise.await finished)
      ~on_cancel:(fun reason ->
        Logs.info (fun f -> f "Cancelling: %s" reason);
        t.state <- "cancelled";
        complete t (Error (`Msg reason)))

  let pp f (k, v) = Fmt.pf f "Set %s to %s" k v

  let auto_cancel = false

  let create () =
    { state = "init"; set_finished = None; next = "unset" }
end

module V = Current.Var(Current.String)

let input = V.create ~name:"input" @@ Ok "bar"

module OC = Current_cache.Output(Publish)

let set oc p k v =
  Current.component "set" |>
  let> v = v in
  OC.set oc p k v

let output env =
  V.set input @@ Ok "bar";
  Current_cache.Db.drop_all "publish";
  let p = Publish.create () in
  let pipeline ~oc () = V.get input |> set oc p "foo" in
  Driver.test env ~name:"cache.output" (fun engine ->
    let oc = OC.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~oc
  ) @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 2 ->
    Alcotest.(check string) "Publish has completed" "bar" p.Publish.state;
    V.set input @@ Ok "baz";
  | 3 ->
    Alcotest.(check string) "Changing to baz" "bar-changing" p.Publish.state;
    V.set input @@ Ok "new";
  | 4 ->
    Alcotest.(check string) "Changed during publish" "bar-changing" p.Publish.state;
    Publish.complete p @@ Error (`Msg "baz failed");
  | 5 ->
    Alcotest.(check string) "First change failed" "bar-changing-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 6 ->
    Alcotest.(check string) "Success" "new" p.Publish.state;
    raise Exit
  | _ ->
    assert false

module Publish2 = struct
  include Publish
  let id = "publish2"
  let auto_cancel = true
end

module OC2 = Current_cache.Output(Publish2)

let set2 oc2 p k v =
  Current.component "set2" |>
  let> v = v in
  OC2.set oc2 p k v

let output_autocancel env =
  V.set input @@ Ok "bar";
  Current_cache.Db.drop_all "publish2";
  let p = Publish2.create () in
  let pipeline ~oc2 () = V.get input |> set2 oc2 p "foo" in
  Driver.test env ~name:"cache.output_autocancel" (fun engine ->
    let oc2 = OC2.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~oc2
  ) @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 2 ->
    Alcotest.(check string) "Publish has completed" "bar" p.Publish.state;
    V.set input @@ Ok "baz";
  | 3 ->
    Alcotest.(check string) "Changing to baz" "bar-changing" p.Publish.state;
    V.set input @@ Ok "new";
  | 4 ->
    ()
  | 5 ->
    Alcotest.(check string) "Changed during publish" "cancelled-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 6 ->
    Alcotest.(check string) "Success" "new" p.Publish.state;
    Driver.rebuild "Set foo to new (completed)";
  | 7 ->
    Alcotest.(check string) "Re-publish has started" "new-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 8 ->
    Alcotest.(check string) "Success" "new" p.Publish.state;
    raise Exit
  | _ ->
    assert false

let output_retry env =
  Current_cache.Db.drop_all "publish2";
  let p = Publish2.create () in
  let pipeline ~oc2 () = set2 oc2 p "foo" (Current.return "value") in
  Driver.test env ~name:"cache.output_retry" (fun engine ->
    let oc2 = OC2.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~oc2
  ) @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    Publish.complete p @@ Error (`Msg "Failed")
  | 2 ->
    Driver.rebuild "Set foo to value: Failed"
  | 3 ->
    Alcotest.(check string) "Publish has restarted" "init-changing-changing" p.Publish.state;
    Publish.complete p @@ Ok ()
  | 4 ->
    Alcotest.(check string) "Publish has completed" "value" p.Publish.state;
    raise Exit
  | _ ->
    assert false

let output_retry_new env =
  Current_cache.Db.drop_all "publish";
  let p = Publish.create () in
  V.set input @@ Ok "1";
  let pipeline ~oc () = set oc p "foo" (V.get input) in
  Driver.test env ~name:"cache.output_retry_new" (fun engine ->
    let oc = OC.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~oc
  ) @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    V.set input @@ Ok "2";
  | 2 ->
    Publish.complete p @@ Error (`Msg "Failed")
  | 3 ->
    Alcotest.(check string) "Publish has restarted" "init-changing-changing" p.Publish.state;
    Publish.complete p @@ Ok ()
  | 4 ->
    Alcotest.(check string) "Publish has completed" "2" p.Publish.state;
    raise Exit
  | _ ->
    assert false

module Latched = struct
  module Key = Current.String
  module Value = Current.String
  module Outcome = Current.String

  type t = (string, string) Hashtbl.t

  let cond = Eio.Condition.create ()
  let mutex = Eio.Mutex.create ()

  let id = "latched"

  let run t job key value =
    Current.Job.start job ~level:Current.Level.Average;
    Eio.Mutex.use_rw ~protect:false mutex (fun () -> Eio.Condition.await cond mutex);
    Hashtbl.replace t key (value ^ "-done");
    if value = "" then Error (`Msg "bad-base")
    else Ok (value ^ "-outcome")

  let pp f (k, v) =
    Fmt.pf f "Set %s to %s" k v

  let auto_cancel = true

  let create () = Hashtbl.create 2

  let latched = true
end

module LC = Current_cache.Generic(Latched)

let build lc p commit base =
  Current.component "build" |>
  let> commit = commit
  and> base = base in
  LC.run lc p commit base

let commit = V.create ~name:"commit" @@ Error (`Msg "(init)")
let base = V.create ~name:"base" @@ Error (`Msg "(init)")

let latched env =
  Current_cache.Db.drop_all "latched";
  let p = Latched.create () in
  V.set base @@ Ok "alpine:3.10";
  V.set commit @@ Ok "r1";
  let result = ref (Error (`Msg ("uninitialised"))) in
  let pipeline ~lc () =
    let+ st = Current.state @@ build lc p (V.get commit) (V.get base) in
    result := st
  in
  Driver.test env ~name:"cache.latched" (fun engine ->
    let lc = LC.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~lc
  ) @@ function
  | 1 ->
    Alcotest.(check result_t) "Op has started" (Error (`Active `Running)) !result;
    Eio.Condition.broadcast Latched.cond;
  | 2 ->
    Alcotest.(check result_t) "3.10 ready" (Ok "alpine:3.10-outcome") !result;
    Alcotest.(check (option string)) "3.10 result" (Some "alpine:3.10-done") (Hashtbl.find_opt p "r1");
    V.set base @@ Ok "alpine:3.11";
  | 3 ->
    Alcotest.(check result_t) "3.10 latched" (Ok "alpine:3.10-outcome") !result;
    Eio.Condition.broadcast Latched.cond;
  | 4 ->
    Alcotest.(check result_t) "3.11 ready" (Ok "alpine:3.11-outcome") !result;
    Alcotest.(check (option string)) "3.11 result" (Some "alpine:3.11-done") (Hashtbl.find_opt p "r1");
    V.set commit @@ Ok "r2";
  | 5 ->
    Alcotest.(check result_t) "Not latched" (Error (`Active `Running)) !result;
    Alcotest.(check (option string)) "No r2 result yet" None (Hashtbl.find_opt p "r2");
    Eio.Condition.broadcast Latched.cond;
  | 6 ->
    Alcotest.(check (option string)) "3.11 result" (Some "alpine:3.11-done") (Hashtbl.find_opt p "r2");
    raise Exit
  | _ ->
    assert false

let latched_autocancel env =
  Current_cache.Db.drop_all "latched";
  let p = Latched.create () in
  V.set base @@ Ok "alpine:3.10";
  V.set commit @@ Ok "r1";
  let result = ref (Error (`Msg ("uninitialised"))) in
  let pipeline ~lc () =
    let+ st = Current.state @@ build lc p (V.get commit) (V.get base) in
    result := st
  in
  Driver.test env ~name:"cache.latched-autocancel" (fun engine ->
    let lc = LC.create ~caps:(Current_cache.caps_of_engine engine) in
    pipeline ~lc
  ) @@ function
  | 1 ->
    Alcotest.(check result_t) "Op has started" (Error (`Active `Running)) !result;
    Eio.Condition.broadcast Latched.cond;
  | 2 ->
    Alcotest.(check result_t) "3.10 ready" (Ok "alpine:3.10-outcome") !result;
    Alcotest.(check (option string)) "3.10 result" (Some "alpine:3.10-done") (Hashtbl.find_opt p "r1");
    V.set base @@ Ok "alpine:3.11";
  | 3 ->
    Alcotest.(check result_t) "3.10 latched" (Ok "alpine:3.10-outcome") !result;
    V.set base @@ Ok "alpine:3.12";
  | 4 ->
    Alcotest.(check result_t) "3.10 latched" (Ok "alpine:3.10-outcome") !result;
    Eio.Condition.broadcast Latched.cond;
  | 5 ->
    Alcotest.(check result_t) "3.10 latched" (Ok "alpine:3.10-outcome") !result;
    Eio.Condition.broadcast Latched.cond;
  | 6 ->
    Alcotest.(check result_t) "3.12 ready" (Ok "alpine:3.12-outcome") !result;
    Alcotest.(check (option string)) "3.12 result" (Some "alpine:3.12-done") (Hashtbl.find_opt p "r1");
    raise Exit
  | _ ->
    assert false

let clear_error env =
  Current_cache.Db.drop_all "latched";
  let p = Latched.create () in
  V.set base @@ Ok "";
  V.set commit @@ Ok "r1";
  let result = ref (Error (`Msg ("uninitialised"))) in
  let lc_ref = ref None in
  let pipeline ~lc () =
    let+ st = Current.state @@ build lc p (V.get commit) (V.get base) in
    result := st
  in
  Driver.test env ~name:"cache.clear_error" (fun engine ->
    let lc = LC.create ~caps:(Current_cache.caps_of_engine engine) in
    lc_ref := Some lc;
    pipeline ~lc
  ) @@ function
  | 1 ->
    Alcotest.(check result_t) "Op has started" (Error (`Active `Running)) !result;
    Eio.Condition.broadcast Latched.cond;
  | 2 ->
    Alcotest.(check result_t) "Bad base failed" (Error (`Msg "bad-base")) !result;
    V.set base @@ Ok "alpine:3.11";
  | 3 ->
    Alcotest.(check result_t) "Failure latched" (Error (`Msg "bad-base")) !result;
    Eio.Condition.broadcast Latched.cond;
  | 4 ->
    Alcotest.(check result_t) "3.11 ready" (Ok "alpine:3.11-outcome") !result;
    V.set base @@ Ok "";
  | 5 ->
    Alcotest.(check result_t) "3.11 still ready" (Ok "alpine:3.11-outcome") !result;
    Eio.Condition.broadcast Latched.cond;
  | 6 ->
    Alcotest.(check result_t) "Bad base failed" (Error (`Msg "bad-base")) !result;
    (match !lc_ref with Some lc -> LC.reset lc ~db:false | None -> ());
    V.set base @@ Ok "alpine:3.11";
  | 7 ->
    Alcotest.(check result_t) "Bad base failed" (Error (`Msg "bad-base")) !result;
    Eio.Condition.broadcast Latched.cond;
  | 8 ->
    Alcotest.(check result_t) "3.11 ready" (Ok "alpine:3.11-outcome") !result;
    raise Exit
  | _ ->
    assert false

let tests env =
  [
    Driver.test_case_gc env "basic"              basic;
    Driver.test_case_gc env "expires"            expires;
    Driver.test_case_gc env "autocancel"         autocancel;
    Driver.test_case_gc env "output"             output;
    Driver.test_case_gc env "output_autocancel"  output_autocancel;
    Driver.test_case_gc env "output_retry"       output_retry;
    Driver.test_case_gc env "output_retry_new"   output_retry_new;
    Driver.test_case_gc env "latched"            latched;
    Driver.test_case_gc env "latched_autocancel" latched_autocancel;
    Driver.test_case_gc env "clear_error"        clear_error;
  ]
