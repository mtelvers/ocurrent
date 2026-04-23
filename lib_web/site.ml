(* A small inline replacement for Session_cohttp.Make, avoiding the
   ocaml-session library's incompatibility with the capnp-rpc 2.x crypto
   stack. Covers just the subset lib_web uses: cookie lookup, cookie
   header generation, session generation/clearing. *)
module Sess = struct
  type backend = Sqlite_session.t

  type t = {
    key : string;
    mutable value : string;
    mutable expiry_period : Int64.t;
    mutable modified : bool;
  }

  let default_period = Sqlite_session.default_period

  let parse_cookies header name =
    match Cohttp.Header.get header "cookie" with
    | None -> None
    | Some s ->
      let cookies = Cohttp.Cookie.Cookie_hdr.extract (Cohttp.Header.of_list ["cookie", s]) in
      List.assoc_opt name cookies

  let of_key backend key =
    match Sqlite_session.get backend key with
    | Ok (value, period) ->
      Ok { key; value; expiry_period = period; modified = false }
    | Error e -> Error e

  let of_header_or_create ?expiry backend name default headers =
    match parse_cookies headers name with
    | Some key ->
      (match of_key backend key with
       | Ok t -> t
       | Error _ ->
         let key = Sqlite_session.generate ?expiry ~value:default backend in
         let period = Option.value expiry ~default:(Sqlite_session.default_period backend) in
         { key; value = default; expiry_period = period; modified = true })
    | None ->
      let key = Sqlite_session.generate ?expiry ~value:default backend in
      let period = Option.value expiry ~default:(Sqlite_session.default_period backend) in
      { key; value = default; expiry_period = period; modified = true }

  let to_cookie_hdrs ?discard:_ ?path ?domain ?secure ?http_only name t =
    if not t.modified then []
    else
      let cookie = (name, t.key) in
      let c =
        Cohttp.Cookie.Set_cookie_hdr.make
          ?path ?domain ?secure ?http_only
          ~expiration:(`Max_age t.expiry_period)
          cookie
      in
      let k, v = Cohttp.Cookie.Set_cookie_hdr.serialize c in
      [k, v]

  let generate backend value =
    let key = Sqlite_session.generate ~value backend in
    let period = Sqlite_session.default_period backend in
    { key; value; expiry_period = period; modified = true }

  let clear backend t =
    Sqlite_session.clear backend t.key

  let set ?expiry backend t value =
    Sqlite_session.set ?expiry backend t.key value;
    t.value <- value;
    t.modified <- true;
    (match expiry with
     | Some p -> t.expiry_period <- p
     | None -> ())
end

class type ['site] raw = object
  method get_raw : 'site -> Cohttp.Request.t -> Cohttp_eio.Server.response
  method post_raw : 'site -> Cohttp.Request.t -> Cohttp_eio.Body.t -> Cohttp_eio.Server.response
  method nav_link : string option
end

type t = {
  name : string;
  authn : (csrf:string -> Uri.t) option;
  has_role : User.t option -> Role.t -> bool;
  secure_cookies : bool;
  http_only: bool;
  session_backend : Sess.backend;
  router : t raw Routes.router;
  nav_links : (string * string) list;   (* Label, path *)
  refresh_pipeline : int option;
}

class type raw_resource = [t] raw

let allow_all _ _ = true

let v ?(name="OCurrent") ?authn ?(secure_cookies=false) ?(http_only=false) ?refresh_pipeline ~has_role routes =
  let db = Lazy.force Current.Db.v in
  let router = Routes.one_of routes in
  let nav_links = routes |> List.filter_map (fun route ->
      let target = Fmt.to_to_string Routes.pp_route route in
      if String.contains target ':' then None else (
        let resource =
          match Routes.match' router ~target with
          | Routes.FullMatch v -> v
          | MatchWithTrailingSlash v -> v
          | NoMatch -> failwith "No match found"
        in
        Option.map (fun label -> (label, target)) resource#nav_link
      )
    ) in
  { name; authn; has_role; secure_cookies; http_only; session_backend = Sqlite_session.create db;
    router; nav_links; refresh_pipeline }
