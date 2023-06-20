type t = {
  name : string;
  image : Image.t;
}

let digest t = t.name ^ "@" ^ Image.digest t.image
let name t = t.name
