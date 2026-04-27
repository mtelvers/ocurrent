val input_installation_webhook : unit -> unit
(** Call this when we get the "installation" event. *)

(* Public API; see Current_git.mli for details of these: *)

type config
(** Pure config produced at Cmdliner-parse time, before any Eio scope. *)

type t
(** A live app constructed inside the engine's Eio scope from a [config]. *)

val create :
  engine:Current.Engine.t ->
  net:[`Generic | `Unix] Eio.Net.ty Eio.Resource.t ->
  config -> t
(** [create ~sw ~net ~clock config] activates [config], constructing an
    HTTPS client and forking the install-monitor daemon on [~sw]. Call this
    from inside [Eio_main.run]/[Eio.Switch.run], before
    {!Current.Engine.create}. *)

val webhook_secret : t -> string
val cmdliner : config Cmdliner.Term.t
val cmdliner_opt : config option Cmdliner.Term.t
val installation : t -> account:string -> int -> Installation.t
val installations : t -> Installation.t list Current.t
