type source = [
  | `No_context
  | `Dir of Fpath.t Current.t
  | `Git of Current_git.Commit.t Current.t
]

type repo_id = string

module type DOCKER = sig
  module Image : sig
    include Current_cache.S.WITH_DIGEST
    include Current_cache.S.WITH_MARSHAL with type t := t

    val of_hash : string -> t
    val hash : t -> string
    val pp : t Fmt.t
  end

  val docker_context : string option
  (** The value passed to Docker via the "--context" argument ([None] for no argument). *)

  val pull :
    ?auth:(string * string) ->
    ?server:string ->
    ?label:string ->
    ?arch:string ->
    schedule:Current_cache.Schedule.t ->
    string -> Image.t Current.t
  (** [pull ~schedule tag] ensures that the latest version of [tag] is cached locally, downloading it if not. *)

  val peek :
    ?label:string ->
    arch:string ->
    schedule:Current_cache.Schedule.t ->
    string -> repo_id Current.t

  val build :
    ?level:Current.Level.t ->
    ?schedule:Current_cache.Schedule.t ->
    ?timeout:Duration.t ->
    ?squash:bool ->
    ?buildx:bool ->
    ?label:string ->
    ?dockerfile:[`File of Fpath.t | `Contents of string] Current.t ->
    ?path:Fpath.t ->
    ?pool:unit Current.Pool.t ->
    ?build_args:string list ->
    pull:bool ->
    source ->
    Image.t Current.t

  val run :
    ?label:string ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    unit Current.t

  val pread :
    ?label:string ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    string Current.t

  val tag : tag:string -> Image.t Current.t -> unit Current.t

  val push : ?auth:(string * string) -> ?server:string -> tag:string -> Image.t Current.t -> repo_id Current.t

  val service : name:string -> image:Image.t Current.t -> unit -> unit Current.t

  val compose : ?pull:bool -> name:string -> contents:string Current.t -> unit -> unit Current.t

  val compose_cli :
    ?pull:bool ->
    ?up_args: string list ->
    name:string ->
    detach:bool ->
    contents:string Current.t ->
    unit -> unit Current.t

  val push_manifest :
    ?auth:(string * string) -> ?server:string -> tag:string -> repo_id Current.t list -> repo_id Current.t

  (** Low-level, primitive-returning API. Useful for building custom components. *)
  module Raw : sig
    val pull :
      schedule:Current_cache.Schedule.t ->
      ?auth:(string * string) ->
      ?server:string ->
      ?arch:string -> string -> Image.t Current.Primitive.t

    val peek :
      schedule:Current_cache.Schedule.t ->
      arch:string -> string -> repo_id Current.Primitive.t

    val build :
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
      ?pool:unit Current.Pool.t ->
      ?run_args:string list ->
      Image.t -> args:string list ->
      unit Current.Primitive.t

    val pread :
      ?pool:unit Current.Pool.t ->
      ?run_args:string list ->
      Image.t -> args:string list ->
      string Current.Primitive.t

    val tag : tag:string -> Image.t -> unit Current.Primitive.t

    val push :
      ?auth:(string * string) -> ?server:string -> tag:string -> Image.t -> repo_id Current.Primitive.t

    val service :
      name:string -> image:Image.t -> unit -> unit Current.Primitive.t

    val compose :
      ?pull:bool -> name:string -> contents:string -> unit -> unit Current.Primitive.t

    val compose_cli :
      ?pull:bool ->
      ?up_args: string list ->
      name:string ->
      detach:bool ->
      contents:string ->
      unit -> unit Current.Primitive.t

    val push_manifest :
      ?auth:(string * string) -> ?server:string -> tag:string -> repo_id list -> repo_id Current.Primitive.t

    (** Building Docker commands. Commands are bound to this instance's [docker_context]. *)
    module Cmd : sig
      type t = string list

      val docker : string list -> t

      val with_container :
        kill_on_cancel:bool ->
        job:Current.Job.t ->
        t ->
        (string -> 'a Current.or_error) ->
        'a Current.or_error

      val pp : t Fmt.t
    end
  end
end
