module User = User
module Role = Role
module Site = Site
module Context = Context
module Utils = Utils

let metrics ~engine = object
  inherit Resource.t

  val! can_get = `Monitor

  method! private get _ctx =
    Current.Engine.(update_metrics engine);
    let data = Prometheus.CollectorRegistry.(collect default) in
    let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
    let headers =
      Cohttp.Header.init_with "Content-Type" "text/plain; version=0.0.4"
      |> Utils.add_security_headers
    in
    Utils.Server.respond_string ~status:`OK ~headers ~body ()
end

let set_confirm ~engine = object
  inherit Resource.t

  method! private post ctx body =
    let data = Uri.query_of_encoded body in
    let config = Current.Engine.config engine in
    match List.assoc_opt "level" data |> Option.value ~default:[] with
    | ["none"] ->
      Current.Config.set_confirm config None;
      Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
    | [level] ->
      begin match Current.Level.of_string level with
        | Error (`Msg msg) -> Context.respond_error ctx `Bad_request msg
        | Ok level ->
          Current.Config.set_confirm config (Some level);
          Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
      end
    | _ -> Context.respond_error ctx `Bad_request "Missing level"
end

let routes engine =
  Routes.[
    nil @--> Main.r ~engine;
    s "index.html" /? nil @--> Main.r ~engine;
    s "pipeline.svg" /? nil @--> Pipeline.r ~engine;
    s "query" /? nil @--> Query.r ~engine;
    s "log-rules" /? nil @--> Log_rules.r ~engine;
    s "log-rules" / s "rules.csv" /? nil @--> Log_rules.rules_csv;
    s "metrics" /? nil @--> metrics ~engine;
    s "set" / s "confirm" /? nil @--> set_confirm ~engine;
    s "jobs" /? nil @--> Jobs.r;
    s "logout" /? nil @--> Resource.logout;
    s "css" / s "ansi.css" /? nil @--> Resource.static ~content_type:"text/css" Ansi.css;
    s "css" / str /? nil @--> Resource.crunch ~content_type:"text/css";
    s "js" / str /? nil @--> Resource.crunch ~content_type:"text/javascript";
    s "img" / str /? nil @--> Resource.crunch;
  ] @ Job.routes ~engine

let handle_request ~site _conn request body =
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri |> Uri.pct_decode in
  Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
  match Routes.match' site.Site.router ~target:path with
  | Routes.NoMatch -> Utils.Server.respond_not_found ()
  | (FullMatch resource) | (MatchWithTrailingSlash resource) ->
    match meth with
    | `GET -> resource#get_raw site request
    | `POST -> resource#post_raw site request body
    | (`HEAD | `PUT | `OPTIONS | `CONNECT | `TRACE | `DELETE | `PATCH | `Other _) ->
      Utils.Server.respond_error ~status:`Bad_request ~body:"Bad method" ()


type t =
  { host : string option;
    port : int }

let pp_mode f { host; port } =
  Fmt.pf f "%a:%d" Fmt.(option ~none:(any "*") string) host port

let default_mode = { host = None; port = 8080 }

let ipaddr_of_host = function
  | None -> Eio.Net.Ipaddr.V4.any
  | Some host ->
    (* Parse via Unix resolver — accepts dotted-quad or hostnames. *)
    let addrs = Unix.getaddrinfo host "" [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
    (match addrs with
     | [] -> Fmt.failwith "Cannot resolve host %S" host
     | { ai_addr = ADDR_INET (addr, _); _ } :: _ ->
       Eio_unix.Net.Ipaddr.of_unix addr
     | _ :: _ -> Fmt.failwith "No IPv4 address for host %S" host)

let run ~net ?(mode=default_mode) site =
  let callback = handle_request ~site in
  let server = Utils.Server.make ~callback () in
  Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
  Eio.Switch.run @@ fun sw ->
  try
    let addr = `Tcp (ipaddr_of_host mode.host, mode.port) in
    let socket = Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net addr in
    Utils.Server.run socket server
      ~on_error:(fun ex -> Log.warn (fun f -> f "Web server error: %a" Fmt.exn ex))
  with
  | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
    Fmt.failwith "Web-server failed.@ Another program is already using this port %a." pp_mode mode

open Cmdliner

let host =
  Arg.value @@
  Arg.(opt (some Arg.string) None) @@
  Arg.info
    ~doc:"The hostname on which to listen for incoming HTTP connections."
    ~docv:"HOST"
    ["host"]

let port =
  Arg.value @@
  Arg.opt Arg.int 8080 @@
  Arg.info
    ~doc:"The port on which to listen for incoming HTTP connections."
    ~docv:"PORT"
    ["port"]

let make host port = { host; port }

let cmdliner =
  Term.(const make $ host $ port)

module Resource = Resource
