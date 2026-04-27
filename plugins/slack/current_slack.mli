type channel

val channel : net:_ Eio.Net.t -> Uri.t -> channel
(** [channel ~net uri] makes a channel using the endpoint URI from Slack
    (create a new app, then add a new webhook using the "Incoming Webhooks"
    page to get the URI). [~net] supplies network capabilities for the HTTPS
    client.
    e.g. [channel ~net @@ Uri.of_string "https://hooks.slack.com/services/..."] *)

val post : channel -> key:string -> string Current.t -> unit Current.t
(** [post channel ~key message] records that [key] is now set to [message], and
    posts [message] to [channel] if it has changed.
    e.g. [post to_dev ~key:"build-status" s] *)
