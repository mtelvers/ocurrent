(** Stashes the engine's switch and env once [Engine.create] has been called,
    so lower-level modules (Job, Config, Process) can access capabilities
    without threading them through every signature.

    This is a pragmatic shortcut for the initial Eio port. Long-term, each
    module should take only the capabilities it needs via its own API. *)

let env : Eio_unix.Stdenv.base option ref = ref None
let sw : Eio.Switch.t option ref = ref None

let init ~sw:s ~env:e =
  env := Some e;
  sw := Some s

let get_env () =
  match !env with
  | Some e -> e
  | None -> failwith "Engine has not been started (Engine.create not called)"

let get_sw () =
  match !sw with
  | Some s -> s
  | None -> failwith "Engine has not been started (Engine.create not called)"

let clock () : _ Eio.Time.clock = Eio.Stdenv.clock (get_env ())
let process_mgr () : _ Eio.Process.mgr = Eio.Stdenv.process_mgr (get_env ())
let fs () : _ Eio.Path.t = Eio.Stdenv.fs (get_env ())
