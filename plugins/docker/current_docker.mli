(** Integration with Docker containers. *)

module S = S

[@@@ocaml.warning "-67"]

module Make () (_ : sig
  val caps : Current_cache.caps
  val git : Current_git.t
  val docker_context : string option
end) : S.DOCKER with module Image = Image
(** [Make () (E)] is a Docker plugin instance bound to the given capabilities,
    git plugin, and Docker context. The unit functor argument makes the
    application generative: each [Make ()] produces a fresh module identity
    with its own caches. *)

module Default () (_ : sig
  val caps : Current_cache.caps
  val git : Current_git.t
end) : S.DOCKER with module Image = Image
(** [Default () (E)] is [Make] with [docker_context] read from the
    [$DOCKER_CONTEXT] environment variable. *)
