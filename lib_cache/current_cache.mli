(** Cache build results in memory and on disk.
    A cache maps keys to values. Looking up a key that isn't known starts a new
    build to create it. *)

module S = S

(** Per-engine capabilities for cache instances. Build via {!caps_of_engine}
    once at startup, then pass to {!Make}/{!Output}/{!Generic} [create]. *)
type caps = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  cache_registry : Current.Engine.cache_registry;
}

val caps_of_engine : Current.Engine.t -> caps
(** Pull the capabilities the cache needs out of an engine. Plugins that
    use {!Make}/{!Output} typically call this once inside their own
    [create ~sw ~env] and pass the resulting [caps] to each cache. *)

(** Configuration settings controlling when to rebuild. *)
module Schedule : sig
  type t

  val v : ?valid_for:Duration.t -> unit -> t
  (** Create a new configuration.
      @param valid_for Consider a cached entry invalid after this long
   *)
end

(** Perform builds with caching. *)
module Make (B : S.BUILDER) : sig
  type t

  val create : caps:caps -> t

  val get : t -> ?schedule:Schedule.t -> B.t -> B.Key.t -> B.Value.t Current.Primitive.t
  (** [get t b k] is a term for the result of building [k]. *)

  val invalidate : t -> B.Key.t -> unit
  (** [invalidate t key] removes [key] from the cache. *)

  val reset : t -> db:bool -> unit
  (** [reset t ~db] clears the cache. Useful for unit-tests.
      @param db Also clear from the on-disk database table. *)
end

(** Publish outputs with caching. *)
module Output (P : S.PUBLISHER) : sig
  type t

  val create : caps:caps -> t

  val set : t -> ?schedule:Schedule.t -> P.t -> P.Key.t -> P.Value.t -> P.Outcome.t Current.Primitive.t
  (** [set t p k v] is a term for the result of setting [k] to [v]. *)

  val reset : t -> db:bool -> unit
  (** [reset t ~db] clears the cache. Useful for unit-tests.
      @param db Also clear from the on-disk database table. *)
end

(** The most general API. {!Make} and {!Output} just specialise this for particular uses. *)
module Generic (Op : S.GENERIC) : sig
  type t

  val create : caps:caps -> t

  val run : t -> ?schedule:Schedule.t -> Op.t -> Op.Key.t -> Op.Value.t -> Op.Outcome.t Current.Primitive.t
  (** [run t k v] is a term for the result of processing [(k, v)]. *)

  val invalidate : t -> Op.Key.t -> unit
  val reset : t -> db:bool -> unit
end

(** Low-level database access (for the web UI and plugins). *)
module Db : sig
  type entry = {
    job_id : string;
    build : int64;            (* Build number (increases for rebuilds). *)
    value : string;
    outcome : string Current.or_error;
    ready : float;            (* When the job was ready to begin. *)
    running : float option;   (* When it actually started running (e.g. after confirmation). *)
    finished : float;         (* When it finished (successfully or not). *)
    rebuild : bool;           (* If [true], then a rebuild was requested. *)
  }

  val init : unit -> unit
  (** Ensure that the database tables have been created.
      This is useful if you need to refer to them in your own SQL. *)

  val query : ?op:string -> ?ok:bool -> ?rebuild:bool -> ?job_prefix:string -> unit -> entry list
  (** Search the database for matching records.
      @param op : if present, restrict to results from the named builder or publisher
      @param ok : if present, restrict results to passing (ok=true) or failing (ok=false) results.
      @param rebuild : if present, restrict results to ones where the rebuild flag matches this.
      @param job_prefix : if present, restrict results to ones where the job ID starts with this string. *)

  val ops : unit -> string list
  (** [ops ()] is the list of operation types that can be passed to [query]. *)

  val history :
    registry:Current.Engine.cache_registry ->
    limit:int ->
    job_id:string ->
    string option * entry list
  (** [history ~registry ~limit ~job_id] returns the in-progress build (if any),
      and the [limit] most recent builds with the same key as [job_id]. *)

  val key : job_id:string -> string option
  (** [key ~job_id] returns the serialised key associated with the job [job_id] if any. *)

  val drop_all : string -> unit
  (** [drop_all op] removes all on-disk records for operation [op]. Useful
      between unit tests that share the same SQLite database. *)
end
