(** Integration with Docker containers. *)

module S = S

val make :
  caps:Current_cache.caps ->
  git:Current_git.t ->
  docker_context:string option ->
  (module S.DOCKER with type Image.t = Image.t)
(** [make ~caps ~git ~docker_context] is a fresh Docker plugin instance
    bound to the given capabilities, git plugin, and Docker context. Each
    call returns a module with its own caches; unpack it with [(val ...)]. *)

val default :
  caps:Current_cache.caps ->
  git:Current_git.t ->
  (module S.DOCKER with type Image.t = Image.t)
(** [default ~caps ~git] is [make] with [docker_context] read from the
    [$DOCKER_CONTEXT] environment variable. *)
