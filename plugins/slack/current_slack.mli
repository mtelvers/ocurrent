type t
(** A live Slack runtime: shared HTTPS client and a result cache scoped to [caps]. *)

val create :
  caps:Current_cache.caps ->
  net:[`Generic | `Unix] Eio.Net.ty Eio.Resource.t ->
  t
(** [create ~caps ~net] builds a fresh Slack runtime. *)

type channel

val channel : t -> Uri.t -> channel
(** [channel t uri] makes a channel bound to [t]'s HTTPS client. The URI
    is the Slack incoming-webhook URL (create a new app, then add a webhook
    via the "Incoming Webhooks" page). *)

val post : t -> channel -> key:string -> string Current.t -> unit Current.t
(** [post t channel ~key message] records that [key] is now set to
    [message], and posts [message] to [channel] if it has changed. *)
