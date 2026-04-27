open Current.Syntax

module PC = Current_cache.Output(Post)

type t = {
  http : Current_http.t;
  cache : PC.t;
}

let create ~engine ~net =
  let caps = Current_cache.caps_of_engine engine in
  { http = Current_http.create ~net; cache = PC.create ~caps }

type channel = Post.t
let channel t uri = { Post.uri; http = t.http }

let post t channel ~key message =
  Current.component "post" |>
  let> message = message in
  PC.set t.cache channel key message
