(** Small Eio.Switch extensions used inside ocurrent. *)

(** [spawn_managed ~parent_sw] is a fresh switch attached to [parent_sw]
    that lives until the returned resolver is resolved (or [parent_sw]
    is torn down). Even after the resolver fires, the inner [Switch.run]
    still waits for child fibers before closing.

    Used by [Current.Monitor] (one switch per activation) and
    [Current_cache.Generic.Instance] (one switch per slot). *)
let spawn_managed ~parent_sw =
  let sw_p, sw_r = Eio.Promise.create () in
  let release_p, release_r = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw:parent_sw (fun () ->
    Eio.Switch.run (fun sw ->
      Eio.Promise.resolve sw_r sw;
      Eio.Promise.await release_p);
    `Stop_daemon);
  Eio.Promise.await sw_p, release_r
