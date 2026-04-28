(** Integration with Git. *)

module Commit_id : sig
  include Set.OrderedType

  val v : repo:string -> gref:string -> hash:string -> t
  (** [v ~repo ~gref ~hash] identifies a commit that can be fetched from [repo]
      using [gref] as the reference name and has hash [hash]. *)

  val repo : t -> string
  (** [repo t] is the Git URI of the repository. *)

  val gref : t -> string

  val hash : t -> string
  (* [hash t] is the Git commit hash. *)

  val equal : t -> t -> bool
  val pp : t Fmt.t

  val pp_user_clone : t Fmt.t
  (** Display a Git command a user could run to get this commit. *)

  val digest : t -> string
end

module Commit : sig
  include Set.OrderedType

  val v : repo:Fpath.t -> id:Commit_id.t -> t
  val id : t -> Commit_id.t
  val hash : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t

  val repo : t -> Fpath.t
  val pp_short : t Fmt.t
  (** [pp_short] shows just the start of the hash. *)

  val marshal : t -> string
  val unmarshal : string -> t
end

type t

val create : caps:Current_cache.caps -> t
(** [create ~caps] builds a Git plugin runtime: fetch/clone caches scoped to [caps]. *)

val clone : t -> schedule:Current_cache.Schedule.t -> ?gref:string -> string -> Commit.t Current.t
(** [clone t ~schedule ~gref uri] evaluates to the head commit of [uri]'s [gref] branch (default: "master"). *)

val fetch : t -> ?token:string -> Commit_id.t Current.t -> Commit.t Current.t
(** [fetch t ?token cid] fetches the commit [cid].
    @param token Optional authentication token for private repositories. *)

val with_checkout :
  t ->
  ?pool:unit Current.Pool.t ->
  job:Current.Job.t ->
  Commit.t ->
  (Fpath.t -> 'a Current.or_error) ->
  'a Current.or_error
(** [with_checkout t ~job c fn] clones [c] to a temporary directory and runs [fn tmpdir].
    When it returns, the directory is deleted. On failure to find the
    commit locally [t]'s fetch cache is invalidated to force a re-fetch.
    @param pool Used to prevent too many clones from happening at once. *)

module Local : sig
  type t
  (** A local Git repository. *)

  val v :
    sw:Eio.Switch.t ->
    process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
    Fpath.t -> t
  (** [v ~sw ~process_mgr path] is the local Git repository at [path].
      [~sw] scopes the fs-watcher fibers; [~process_mgr] is used to spawn
      [git] subprocesses. *)

  val head : t -> [`Commit of Commit_id.t | `Ref of string ] Current.t
  (** [head] is the current branch ref (e.g. "/refs/heads/master"). *)

  val head_commit : t -> Commit.t Current.t
  (** [head_commit] is the commit at the head of the current branch. *)

  val commit_of_ref : t -> string -> Commit.t Current.t
  (** [commit_of_ref t gref] evaluates to the commit at the head of [gref].
      e.g. [commit_of_ref t "/refs/heads/master"] *)

  val repo : t -> Fpath.t
end
