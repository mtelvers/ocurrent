val init_logging : unit -> unit

val test :
  Eio_unix.Stdenv.base ->
  ?config:Current.Config.t ->
  ?clock:Eio_mock.Clock.t ->
  ?final_stats:Current_term.S.stats ->
  name:string ->
  (Current.Engine.t -> unit -> unit Current.t) ->
  (int -> unit) ->
  unit
(** [test env ~name pipeline_factory actions] runs the engine, calling
    [pipeline_factory engine] once to obtain the actual pipeline (so the
    test can build cache instances from [engine]'s caps). After each
    iteration, it calls [actions i] where [i] is the number of the next
    step ([1] on the first call). If [actions i] raises [Exit] the test
    finishes. [?clock] overrides the engine's clock with a mock. *)

val cancel : string -> unit
(** [cancel msg] cancels the job named [msg]. *)

val rebuild : string -> unit
(** [rebuild msg] triggers a rebuild of the job named [msg]. *)

val test_case_gc :
  Eio_unix.Stdenv.base ->
  string ->
  (Eio_unix.Stdenv.base -> unit) ->
  unit Alcotest.test_case
