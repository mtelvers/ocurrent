open Current.Syntax

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let repositories_total =
    let help = "Total number of active repositories" in
    Gauge.v_label ~label_name:"account" ~help ~namespace ~subsystem "repositories_total"
end

type repository_metadata = {
  archived : bool;
}

type t = {
  iid : int;
  account : string;
  api : Api.t;
  repos : (Api.Repo.t * repository_metadata) list Current.Monitor.t;
}

let installation_repositories_cond = Eio.Condition.create ()
let installation_repositories_mutex = Eio.Mutex.create ()

let input_installation_repositories_webhook () =
  Eio.Condition.broadcast installation_repositories_cond

let pp f t = Fmt.string f t.account

let account t = t.account

let compare a b = compare a.iid b.iid

let list_repositories_endpoint = Uri.of_string "https://api.github.com/installation/repositories"

let next headers =
  headers
  |> Cohttp.Header.get_links
  |> List.find_opt (fun (link : Cohttp.Link.t) ->
      List.exists (fun r -> r = Cohttp.Link.Rel.next) link.arc.relation
    )
  |> Option.map (fun link -> link.Cohttp.Link.target)

let list_repositories ~api ~token ~account =
  let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ token) in
  let headers = Cohttp.Header.add headers "accept" "application/vnd.github.machine-man-preview+json" in
  let rec aux uri =
    Log.debug (fun f -> f "Get repositories for %S from %a" account Uri.pp uri);
    let resp, body = Http.get ~headers uri in
    match Cohttp.Response.status resp with
    | `OK ->
      let json = Yojson.Safe.from_string body in
      Log.debug (fun f -> f "@[<v2>Got response:@,%a@]" Yojson.Safe.pp json);
      let open Yojson.Safe.Util in
      let repos =
        json
        |> member "repositories"
        |> to_list
        |> List.map (fun r ->
            let name = r |> member "name" |> to_string in
            let archived = r |> member "archived" |> to_bool in
            let metadata = { archived } in
            (api, Repo_id.{ owner = account; name }), metadata
          )
      in
      begin match next (Cohttp.Response.headers resp) with
        | None -> repos
        | Some target ->
          let next_repos = aux target in
          repos @ next_repos
      end
    | err -> Fmt.failwith "@[<v2>Error accessing GitHub installation API at %a: %s@,%s@]"
               Uri.pp uri
               (Cohttp.Code.string_of_status err)
               body
  in
  let repos = aux list_repositories_endpoint in
  Prometheus.Gauge.set (Metrics.repositories_total account) (float_of_int (List.length repos));
  repos

let v ~iid ~account ~api =
  let read () =
    match Api.get_token api with
    | Error (`Msg m) -> failwith m
    | Ok token ->
      try Ok (list_repositories ~api ~token ~account)
      with ex ->
        Log.warn (fun f -> f "Error reading GitHub installations (will retry in 30s): %a" Fmt.exn ex);
        Eio.Time.sleep (Current.Engine_env.clock ()) 30.0;
        Ok (list_repositories ~api ~token ~account)
  in
  let watch refresh =
    let stop = ref false in
    Eio.Fiber.fork_daemon ~sw:(Current.Engine_env.get_sw ()) (fun () ->
      let rec aux () =
        if !stop then `Stop_daemon
        else begin
          Eio.Mutex.use_rw ~protect:false installation_repositories_mutex (fun () ->
            Eio.Condition.await installation_repositories_cond installation_repositories_mutex);
          refresh ();
          aux ()
        end
      in
      aux ()
    );
    fun () ->
      stop := true;
      Eio.Condition.broadcast installation_repositories_cond
  in
  let pp f = Fmt.string f account in
  let repos = Current.Monitor.create ~read ~watch ~pp in
  { iid; account; api; repos }

let api t = t.api

let repositories ?(include_archived=false) t =
  Current.component "list repos" |>
  let> t = t in
  let process =
    if include_archived then List.map fst
    else
      List.filter_map (function
          | _, { archived = true } -> None
          | repo, { archived = false } -> Some repo
        )
  in
  Current.Monitor.get t.repos
  |> Current.Primitive.map_result (Result.map process)
