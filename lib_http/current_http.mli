(** HTTPS client helper for OCurrent plugins.

    Construct a client once at plugin startup with [create ~net], then use
    [get]/[post]/[patch] as request methods. The client is thread-safe and
    can be shared. *)

type t
(** A configured HTTPS client (cohttp-eio + tls-eio + system CA bundle). *)

val create : net:_ Eio.Net.t -> t
(** [create ~net] is a fresh HTTPS client using [net] for outbound TCP. *)

val get :
  t ->
  ?headers:Cohttp.Header.t ->
  Uri.t ->
  Cohttp.Response.t * string
(** [get t ?headers uri] performs an HTTPS GET and returns the response and
    body as a string. *)

val post :
  t ->
  ?headers:Cohttp.Header.t ->
  ?body:string ->
  Uri.t ->
  Cohttp.Response.t * string
(** [post t ?headers ?body uri] performs an HTTPS POST. *)

val patch :
  t ->
  ?headers:Cohttp.Header.t ->
  ?body:string ->
  Uri.t ->
  Cohttp.Response.t * string
(** [patch t ?headers ?body uri] performs an HTTPS PATCH. *)
