(** Integration with Docker containers. *)

module S = S

module Default : S.DOCKER
(** The default Docker engine (from the [$DOCKER_HOST] environment variable). *)

module Make (_ : S.HOST) : S.DOCKER
(** The docker engine running on [Host]. *)

(** Low-level API. This is useful for building custom components.
    Construct a single {!t} via {!create} from inside the engine's Eio scope,
    then pass it to each call. *)
module Raw : sig
  module Image = Image

  type t

  val create : engine:Current.Engine.t -> git:Current_git.t -> t

  val pull :
    t ->
    docker_context:string option ->
    schedule:Current_cache.Schedule.t ->
    ?auth:(string * string) ->
    ?server:string ->
    ?arch:string -> string -> Image.t Current.Primitive.t

  val peek :
    t ->
    docker_context:string option ->
    schedule:Current_cache.Schedule.t ->
    arch:string -> string -> S.repo_id Current.Primitive.t

  val build :
    t ->
    docker_context:string option ->
    ?level:Current.Level.t ->
    ?schedule:Current_cache.Schedule.t ->
    ?timeout:Duration.t ->
    ?squash:bool ->
    ?buildx:bool ->
    ?dockerfile:[`File of Fpath.t | `Contents of string] ->
    ?path:Fpath.t ->
    ?pool:unit Current.Pool.t ->
    ?build_args:string list ->
    pull:bool ->
    [ `Git of Current_git.Commit.t | `Dir of Fpath.t | `No_context ] ->
    Image.t Current.Primitive.t

  val run :
    t ->
    docker_context:string option ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t -> args:string list ->
    unit Current.Primitive.t

  val pread :
    t ->
    docker_context:string option ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t -> args:string list ->
    string Current.Primitive.t

  val tag :
    t ->
    docker_context:string option ->
    tag:string -> Image.t -> unit Current.Primitive.t

  val push :
    t ->
    docker_context:string option ->
    ?auth:(string * string) -> ?server:string -> tag:string -> Image.t -> S.repo_id Current.Primitive.t

  val service :
    t ->
    docker_context:string option ->
    name:string -> image:Image.t -> unit -> unit Current.Primitive.t

  val compose :
    t ->
    ?pull:bool ->
    docker_context:string option ->
    name:string ->
    contents:string -> unit -> unit Current.Primitive.t

  val compose_cli :
    t ->
    ?pull:bool ->
    ?up_args: string list ->
    docker_context:string option ->
    name:string ->
    detach:bool ->
    contents:string ->
    unit -> unit Current.Primitive.t

  val push_manifest :
    t -> ?auth:(string * string) -> ?server:string -> tag:string -> S.repo_id list -> S.repo_id Current.Primitive.t

  (** Building Docker commands. *)
  module Cmd : sig
    type t = string list

    val docker : string list -> docker_context:string option -> t

    val with_container :
      docker_context:string option ->
      kill_on_cancel:bool ->
      job:Current.Job.t ->
      t ->
      (string -> 'a Current.or_error) ->
      'a Current.or_error

    val pp : t Fmt.t
  end
end
