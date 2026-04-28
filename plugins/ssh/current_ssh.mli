type t

val create : caps:Current_cache.caps -> t
(** [create ~caps] builds a runtime with a result cache scoped to [caps]. *)

val run : t -> schedule:Current_cache.Schedule.t -> key:string -> string -> string list Current.t -> unit Current.t
(** [run t ~schedule ~key host args] records that [key] is now set to [args], and
    runs ssh host [args] if it has changed.
    e.g. [run t ~schedule ~key:"my-ls" "fqdn" ["ls"; "-l"]] *)
