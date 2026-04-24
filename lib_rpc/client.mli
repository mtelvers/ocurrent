(** Unified RPC client for OCurrent pipelines.

    This module provides client operations and cmdliner integration for
    interacting with OCurrent pipelines via Cap'n Proto RPC. *)

(** {2 Client Operations}

    These functions perform individual RPC operations. They can be used
    directly if you need programmatic access rather than CLI.
    Each operation is synchronous: it runs on the current fiber and
    suspends while waiting for the remote call. *)

module Ops : sig
  val overview : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** Show pipeline statistics and state. *)

  val jobs : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** List active jobs. *)

  val status : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [status engine job_id] shows the status of a specific job. *)

  val log : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [log engine job_id] streams the log of a job. *)

  val cancel : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [cancel engine job_id] cancels a running job. *)

  val rebuild : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [rebuild engine job_id] rebuilds a job and streams its log. *)

  val start : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [start engine job_id] approves early start for a waiting job. *)

  val query :
    Engine.t ->
    op:string option ->
    ok:bool option ->
    rebuild:bool option ->
    job_prefix:string option ->
    (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** Query job history with optional filters. *)

  val ops : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** List operation types. *)

  val dot : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** Output pipeline as DOT graph. *)

  val confirm : Engine.t -> string option -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [confirm engine None] gets the current level; [confirm engine (Some level)] sets it. *)

  val rebuild_all : Engine.t -> string list -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** Rebuild multiple jobs. *)
end

(** {2 Connection Helpers} *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  Uri.t ->
  Engine.t
(** [connect ~sw ~net cap_uri] connects to an engine using a capability URI. *)

val with_engine :
  net:_ Eio.Net.t ->
  Uri.t ->
  (Engine.t -> 'a) ->
  'a
(** [with_engine ~net cap_uri f] connects to an engine, runs [f], then
    releases the capability. Opens its own switch for the connection's
    lifetime. *)

(** {2 Cmdliner Integration} *)

module Cmdliner : sig
  val cap_uri : Uri.t Cmdliner.Term.t
  (** Term for the --cap option to specify the capability file/URI. *)

  val make_subcommands : Uri.t Cmdliner.Term.t -> unit Cmdliner.Cmd.t list
  (** [make_subcommands cap_uri] builds the client subcommands parameterised
      by the cap_uri term. *)

  val cmd : string -> string -> unit Cmdliner.Cmd.t
  (** [cmd name version] is a command group with all client subcommands,
      using the standard --cap option. *)
end
