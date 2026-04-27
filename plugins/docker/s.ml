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

  type t

  val create : engine:Current.Engine.t -> git:Current_git.t -> t

  val docker_context : string option

  val pull :
    t ->
    ?auth:(string * string) ->
    ?server:string ->
    ?label:string ->
    ?arch:string ->
    schedule:Current_cache.Schedule.t ->
    string -> Image.t Current.t
  (** [pull t ~schedule tag] ensures that the latest version of [tag] is cached locally, downloading it if not. *)

  val peek :
    t ->
    ?label:string ->
    arch:string ->
    schedule:Current_cache.Schedule.t ->
    string -> repo_id Current.t

  val build :
    t ->
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
    t ->
    ?label:string ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    unit Current.t

  val pread :
    t ->
    ?label:string ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    string Current.t

  val tag : t -> tag:string -> Image.t Current.t -> unit Current.t

  val push : t -> ?auth:(string * string) -> ?server:string -> tag:string -> Image.t Current.t -> repo_id Current.t

  val service : t -> name:string -> image:Image.t Current.t -> unit -> unit Current.t

  val compose : t -> ?pull:bool -> name:string -> contents:string Current.t -> unit -> unit Current.t

  val compose_cli :
    t ->
    ?pull:bool ->
    ?up_args: string list ->
    name:string ->
    detach:bool ->
    contents:string Current.t ->
    unit -> unit Current.t

  val push_manifest :
    t -> ?auth:(string * string) -> ?server:string -> tag:string -> repo_id Current.t list -> repo_id Current.t
end

module type HOST = sig
  val docker_context : string option
  (** The value to pass to Docker via the "--context" argument ([None] for no argument). *)
end
