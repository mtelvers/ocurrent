open Current.Syntax

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let installations_total =
    let help = "Total number of active app installations" in
    Gauge.v ~help ~namespace ~subsystem "installations_total"
end


(* Fires when the list should be updated. *)
let installations_changed_cond = Eio.Condition.create ()
let installations_changed_mutex = Eio.Mutex.create ()

let input_installation_webhook () = Eio.Condition.broadcast installations_changed_cond

let list_installations_endpoint =
  Uri.of_string "https://api.github.com/app/installations"

let access_tokens_endpoint id =
  Uri.of_string (Fmt.str "https://api.github.com/app/installations/%d/access_tokens" id)

module Int_map = Map.Make(Int)

module Installs = Current.Var(struct
    type t = Installation.t Int_map.t
    let equal = Int_map.equal (fun _ _ -> true)
    let pp = Fmt.using (fun t -> Int_map.bindings t |> List.map fst) Fmt.(Dump.list int)
  end)

module Allowlist : sig

  type t

  val of_list : string list -> t

  val mem : string -> t -> bool

end = struct
  type t = string list
  let of_list = List.map String.lowercase_ascii
  let mem name l = List.exists ((=) (String.lowercase_ascii name)) l
end

type t = {
  app_id : string;
  key : Mirage_crypto_pk.Rsa.priv;
  allowlist : Allowlist.t;      (* Accounts which can use this app. *)
  installations : Installs.t;
  webhook_secret : string; (* Shared secret for validating webhooks from GitHub *)
}

let webhook_secret t = t.webhook_secret

let http { app_id; key; _ } op uri =
  let iat = truncate @@ Unix.gettimeofday () in
  let jwt = Token.encode ~key ~iat ~app_id in
  let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ jwt) in
  let headers = Cohttp.Header.add headers "accept" "application/vnd.github.machine-man-preview+json" in
  Log.debug (fun f -> f "API call on %a" Uri.pp uri);
  let resp, body = op ~headers uri in
  match Cohttp.Response.status resp with
  | `OK | `Created ->
    let json = Yojson.Safe.from_string body in
    Log.debug (fun f -> f "@[<v2>Got response:@,%a@]" Yojson.Safe.pp json);
    resp, json
  | err -> Fmt.failwith "@[<v2>Error accessing GitHub App API at %a: %s@,%s@]"
             Uri.pp uri
             (Cohttp.Code.string_of_status err)
             body

let get ~headers uri = Http.get ~headers uri
let post ~headers uri = Http.post ~headers uri

let minute = 60.0

let get_token app iid =
  let uri = access_tokens_endpoint iid in
  let now = Unix.gettimeofday () in
  let _resp, json = http app post uri in
  let open Yojson.Safe.Util in
  let token = Ok (json |> member "token" |> to_string) in
  (* The token is valid for 60 minutes, so request a new one after 50 minutes. *)
  let expiry = Some (now +. 50.0 *. minute) in
  Api.{ token; expiry }

let next headers =
  headers
  |> Cohttp.Header.get_links
  |> List.find_opt (fun (link : Cohttp.Link.t) ->
      List.exists (fun r -> r = Cohttp.Link.Rel.next) link.arc.relation
    )
  |> Option.map (fun link -> link.Cohttp.Link.target)

let get_installations app =
  let open Current.Result.Syntax in
  try
    let rec aux uri =
      let resp, json = http app get uri in
      let open Yojson.Safe.Util in
      let installs =
        json |> to_list |> List.filter_map (fun json ->
            let id = json |> member "id" |> to_int in
            let account = json |> member "account" |> member "login" |> to_string in
            if Allowlist.mem account app.allowlist then (
              Log.info (fun f -> f "Found installation %d for %S" id account);
              let repository_selection = json |> member "repository_selection" |> to_string in
              match repository_selection with
              | "selected" -> Some (id, account)
              | "all" ->
                Log.warn (fun f -> f "Installation %S has selected all repositories - skipping as probably a mistake" account);
                None
              | x ->
                Log.warn (fun f -> f "Installation %S has unknown repository_selection %S - skipping" account x);
                None
            ) else (
              Log.warn (fun f -> f "Installation %d for %S : account not on allowlist!" id account);
              None
            )
          )
      in
      match next (Cohttp.Response.headers resp) with
      | None -> Ok installs
      | Some target ->
        let* next_installs = aux target in
        Ok (installs @ next_installs)
    in
    aux list_installations_endpoint
  with ex ->
    Error (`Msg (Fmt.str "Failed to get GitHub installations: %a" Fmt.exn ex))

let installation t ~account iid =
  let api = Api.v ~get_token:(fun () -> get_token t iid) ~account:("i-" ^ account) ~app_id:t.app_id ~webhook_secret:t.webhook_secret () in
  Installation.v ~api ~account ~iid

module Int_set = Set.Make(Int)

let remove_stale_installations new_ids =
  let new_ids = new_ids |> List.map fst |> Int_set.of_list in
  Int_map.filter (fun key _ -> Int_set.mem key new_ids)

let monitor_installations t () =
  let rec aux () =
    let ids = get_installations t in
    begin match ids with
      | Ok ids ->
        Prometheus.Gauge.set Metrics.installations_total (float_of_int (List.length ids));
        Installs.update t.installations (fun old_map ->
            let old_map = match old_map with Ok x -> x | Error _ -> Int_map.empty in
            (* Merge in new installations. Reuse existing Apis so we don't keep refreshing tokens, etc. *)
            ids |> ListLabels.fold_left ~init:old_map ~f:(fun acc (iid, account) ->
                if Int_map.mem iid acc then acc
                else Int_map.add iid (installation t iid ~account) acc
              )
            |> remove_stale_installations ids
            |> Stdlib.Result.ok
          );
      | Error (`Msg m) ->
        Log.warn (fun f -> f "Failed to update list of installations: %s" m)
    end;
    Eio.Time.sleep (Current.Engine_env.clock ()) 60.0;   (* Wait at least 1m between updates *)
    Eio.Mutex.use_rw ~protect:false installations_changed_mutex (fun () ->
      Eio.Condition.await installations_changed_cond installations_changed_mutex);
    aux ()
  in
  aux ()

let installations t =
  let+ apis = Installs.get t.installations in
  apis |> Int_map.bindings |> List.map snd

(* Command-line options *)

let make_config app_id private_key_file allowlist webhook_secret_file =
  let allowlist = Allowlist.of_list allowlist in
  let data = Api.read_file private_key_file in
  let webhook_secret = Api.read_file webhook_secret_file in
  match X509.Private_key.decode_pem data with
    | Error (`Msg msg) -> Fmt.failwith "Failed to parse secret key!@ %s" msg
    | Ok (`RSA key) ->
      let installations = Installs.create ~name:"installations" (Error (`Active `Running)) in
      let t = { app_id; key; allowlist; installations; webhook_secret } in
      Eio.Fiber.fork_daemon ~sw:(Current.Engine_env.get_sw ()) (fun () ->
        (try monitor_installations t ()
         with ex -> Log.err (fun f -> f "monitor_installations failed: %a" Fmt.exn ex));
        `Stop_daemon
      );
      t
    | Ok _ -> Fmt.failwith "Unsupported private key type" [@@warning "-11"]

open Cmdliner

let make_config_opt app_id private_key_file allowlist webhook_secret : t option Term.ret =
  match app_id, private_key_file, allowlist with
  | None, None, _ -> `Ok None
  | Some app_id, Some private_key_file, Some allowlist -> `Ok (Some (make_config app_id private_key_file allowlist webhook_secret))
  | Some _, Some _, None -> `Error (true, "--github-account-allowlist is required with --github-app-id")
  | Some _, None, _ -> `Error (true, "--github-private-key-file is required with --github-app-id")
  | None, Some _, _ -> `Error (true, "--github-app-id is required with --github-private-key-file")

let private_key_file =
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"A file containing the GitHub app's RSA private key."
    ~docv:"PATH"
    ["github-private-key-file"]

let app_id =
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"The GitHub app's (integer) ID."
    ~docv:"ID"
    ["github-app-id"]

let allowlist =
  Arg.opt Arg.(some (list string)) None @@
  Arg.info
    ~doc:"A comma-separated list of allowed GitHub accounts."
    ~docv:"ACCOUNTS"
    ["github-account-allowlist"]

let cmdliner =
  Term.(const make_config $ Arg.required app_id $ Arg.required private_key_file $ Arg.required allowlist $ Api.webhook_secret_file)

let cmdliner_opt =
  Term.(ret (const make_config_opt $ Arg.value app_id $ Arg.value private_key_file $ Arg.value allowlist $ Api.webhook_secret_file))
