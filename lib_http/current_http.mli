(** HTTPS client helper for OCurrent plugins. *)

val get :
  ?headers:Cohttp.Header.t ->
  Uri.t ->
  Cohttp.Response.t * string
(** [get ?headers uri] performs an HTTPS GET request to [uri] and returns the
    response together with the body as a string.  Requires
    {!Current.Engine_env} to have been initialised. *)

val post :
  ?headers:Cohttp.Header.t ->
  ?body:string ->
  Uri.t ->
  Cohttp.Response.t * string
(** [post ?headers ?body uri] performs an HTTPS POST request. *)

val patch :
  ?headers:Cohttp.Header.t ->
  ?body:string ->
  Uri.t ->
  Cohttp.Response.t * string
(** [patch ?headers ?body uri] performs an HTTPS PATCH request. *)
