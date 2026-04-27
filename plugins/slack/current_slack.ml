open Current.Syntax

module PC = Current_cache.Output(Post)

type channel = Post.t
let channel ~net uri = { Post.uri; http = Current_http.create ~net }

let post channel ~key message =
  Current.component "post" |>
  let> message = message in
  PC.set channel key message
