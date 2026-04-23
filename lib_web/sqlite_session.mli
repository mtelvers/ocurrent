(** A minimal sqlite-backed session store for OCurrent's web UI.

    Originally this was a [Session.S.Now] backend used with the ocaml-session
    library. Under the Eio port we inline a small subset of that library's
    API here to avoid dragging its old crypto deps (nocrypto, cstruct < 6.1)
    which conflict with capnp-rpc 2.x's newer crypto stack. *)

type key = string
type value = string
type period = Int64.t

type error = Not_found | Not_set

type t

val create : Current.Db.t -> t

val default_period : t -> period

val generate : ?expiry:period -> ?value:value -> t -> key
(** [generate ?expiry ?value t] creates a new session with a random key. *)

val get : t -> key -> (value * period, error) result

val set : ?expiry:period -> t -> key -> value -> unit

val clear : t -> key -> unit
