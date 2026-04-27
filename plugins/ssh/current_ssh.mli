type t

val create : engine:Current.Engine.t -> t
(** [create ~engine] builds a runtime inside the engine's Eio scope, with
    a per-engine result cache. *)

val run : t -> schedule:Current_cache.Schedule.t -> key:string -> string -> string list Current.t -> unit Current.t
(** [run t ~schedule ~key host args] records that [key] is now set to [args], and
    runs ssh host [args] if it has changed.
    e.g. [run t ~schedule ~key:"my-ls" "fqdn" ["ls"; "-l"]] *)
