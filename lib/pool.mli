type 'a t

type priority = [ `High | `Low ]

val create : label:string -> int -> unit t

val of_fn :
  label : string ->
  (priority:priority -> sw:Eio.Switch.t -> 'a) ->
  'a t

val get :
  'a t ->
  priority:priority ->
  sw:Eio.Switch.t ->
  ?register_cancel:((unit -> unit) -> unit) ->
  unit ->
  'a
(** [get ~priority ~sw t ()] waits for a resource and returns it.
    The fiber suspends until a resource is available. If the fiber is
    cancelled while waiting, the request is removed from the queue.
    The resource will be returned to the pool when [sw] is released.
    @param register_cancel is an opportunity for the caller to hook an
       out-of-band cancellation trigger into the pool wait. If given, the
       pool calls [register_cancel cancel] before suspending; if the caller
       later invokes [cancel], the pool wait aborts with the usual
       "Cancelled waiting for resource" error. *)

val pp : _ t Fmt.t
