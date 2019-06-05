(* Utilties for generating dot files. *)

val node : Format.formatter -> ?style:string -> ?shape:string -> ?bg:string -> int -> string -> unit
val edge : Format.formatter -> ?style:string -> ?color:string -> int -> int -> unit
