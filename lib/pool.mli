type 'a t

type priority = [ `High | `Low ]

val create : label:string -> int -> unit t

val of_fn :
  label : string ->
  (priority:priority -> switch:Switch.t -> 'a) ->
  'a t

val get :
  'a t ->
  priority:priority ->
  switch:Switch.t ->
  'a
(** [get ~priority ~switch t] waits for a resource and returns it.
    The fiber suspends until a resource is available. If the fiber is
    cancelled while waiting, the request is removed from the queue.
    The resource will be returned to the pool when [switch] is turned off. *)

val pp : _ t Fmt.t
