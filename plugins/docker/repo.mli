type t = {
  name : string;
  image : Image.t;
}

val digest : t -> string
val name : t -> string
