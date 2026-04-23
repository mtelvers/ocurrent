module Server = Current_web.Utils.Server

type t = {
  client_id : string;
  client_secret: string;
  scopes : string list;
  redirect_uri : string;
} [@@deriving yojson]

let v ?(scopes=["read_user"]) ~client_id ~client_secret ~redirect_uri () =
  { client_id; client_secret; scopes; redirect_uri }

(* Known GitLab OAuth scopes. See
   https://docs.gitlab.com/ee/integration/oauth_provider.html *)
let known_scopes = [
  "api"; "read_api"; "read_user"; "read_repository"; "write_repository";
  "read_registry"; "write_registry"; "sudo"; "openid"; "profile"; "email";
]

exception ScopeOfString of string

let validate_scope scope =
  if List.mem scope known_scopes then scope
  else raise @@ ScopeOfString ("Invalid OAuth scope: " ^ scope)

let make_login_uri t ~csrf =
  let scopes = String.concat " " (List.map validate_scope t.scopes) in
  Uri.with_query'
    (Uri.of_string "https://gitlab.com/oauth/authorize")
    [
      "client_id", t.client_id;
      "redirect_uri", t.redirect_uri;
      "response_type", "code";
      "state", csrf;
      "scope", scopes;
    ]

(* POST https://gitlab.com/oauth/token with the authorization code to get
   an access token.  Returns [Some token] on success. *)
let get_access_token t code =
  let uri = Uri.of_string "https://gitlab.com/oauth/token" in
  let form =
    Uri.encoded_of_query [
      "client_id",     [t.client_id];
      "client_secret", [t.client_secret];
      "code",          [code];
      "grant_type",    ["authorization_code"];
      "redirect_uri",  [t.redirect_uri];
    ]
  in
  let headers = Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded" in
  let resp, body = Current_http.post ~headers ~body:form uri in
  match Cohttp.Response.status resp with
  | `OK ->
    let json = Yojson.Safe.from_string body in
    Some (Yojson.Safe.Util.(json |> member "access_token" |> to_string))
  | _ -> None

(* GET https://gitlab.com/api/v4/user with the bearer token. *)
let get_user token =
  let headers = Cohttp.Header.init_with "Authorization" ("Bearer " ^ token) in
  let uri = Uri.of_string "https://gitlab.com/api/v4/user" in
  let resp, body = Current_http.get ~headers uri in
  match Cohttp.Response.status resp with
  | `OK ->
    let user = Gitlab_types_j.current_user_of_string body in
    Ok ("gitlab:" ^ user.Gitlab_types_t.current_user_username)
  | status -> Error (status, body)

let example_config () =
  v ~client_id:"..." ~client_secret:"..." ~redirect_uri:"..." ()
  |> to_yojson
  |> Yojson.Safe.pretty_to_string

let configuration_howto ctx =
  Current_web.Context.respond_ok ctx Tyxml.Html.[
      p [ txt "GitLab single-sign-on has not been configured." ];
      p [
        txt "Start the service with ";
        code [txt "--gitlab-oauth path.json"];
        txt ", where the file contains:";
      ];
      pre [ txt (example_config ()) ]
    ]

let login t : Current_web.Resource.t = object
  method get_raw site request =
    let ctx = Current_web.Context.of_request ~site request in
    match t with
    | None -> configuration_howto ctx
    | Some t ->
      let uri = Cohttp.Request.uri request in
      match Uri.get_query_param uri "code", Uri.get_query_param uri "state" with
      | None, _ -> Server.respond_error ~status:`Bad_request ~body:"Missing code" ()
      | _, None -> Server.respond_error ~status:`Bad_request ~body:"Missing state" ()
      | Some code, Some state ->
        if state <> Current_web.Context.csrf ctx then (
          Server.respond_error ~status:`Bad_request ~body:"Bad CSRF token" ()
        ) else (
          match get_access_token t code with
          | None ->
            Server.respond_error ~status:`Internal_server_error ~body:"Failed to get token" ()
          | Some token ->
            match get_user token with
            | Error (status, msg) ->
              Log.warn (fun f -> f "Failed to get user details from GitLab: %s: %s" (Cohttp.Code.string_of_status status) msg);
              Server.respond_error ~status:`Internal_server_error ~body:"Failed to get user details" ()
            | Ok user ->
              Log.info (fun f -> f "Successful login for %S" user);
              match Current_web.User.v user with
              | Error (`Msg m) ->
                Log.warn (fun f -> f "Failed to create user: %s" m);
                Server.respond_error ~status:`Bad_request ~body:"Bad user" ()
              | Ok user ->
                Current_web.Context.set_user ctx user
        )

  method post_raw _ _ _ =
    Server.respond_error ~status:`Bad_request ~body:"Bad method" ()

  method nav_link = None
end

open Cmdliner

let oauth_config =
  Arg.value @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"The JSON file containing the GitLab OAuth configuration"
    ~docv:"PATH"
    ["gitlab-oauth"]

let make_config path =
  match Yojson.Safe.from_file path with
  | exception ex -> Fmt.failwith "Invalid JSON in %s:@,%a" path Fmt.exn ex
  | json ->
    json
    |> of_yojson
    |> function
    | Ok x -> x
    | Error msg ->
      Fmt.failwith "Invalid GitLab OAuth configuration: %s@.Expected: %s" msg (example_config ())

let cmdliner =
  Term.(const (Option.map make_config) $ oauth_config)
