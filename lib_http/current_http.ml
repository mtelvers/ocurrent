(* HTTPS client helper shared between OCurrent plugins.  Wraps
   cohttp-eio + tls-eio with a small API shaped like cohttp-lwt-unix's
   so that plugins can port mechanically from the Lwt version.
   Trusts the system CA bundle via Ca_certs. *)

let authenticator = lazy (
  match Ca_certs.authenticator () with
  | Ok a -> a
  | Error (`Msg m) -> Fmt.failwith "Failed to load system CA certs: %s" m
)

let tls_config = lazy (
  let authenticator = Lazy.force authenticator in
  match Tls.Config.client ~authenticator () with
  | Ok c -> c
  | Error (`Msg m) -> Fmt.failwith "Failed to build TLS config: %s" m
)

let https_handler uri raw_flow =
  let host =
    Uri.host uri
    |> Option.map (fun h -> Domain_name.host_exn (Domain_name.of_string_exn h))
  in
  Tls_eio.client_of_flow ?host (Lazy.force tls_config) raw_flow

type t = Cohttp_eio.Client.t

let create ~net = Cohttp_eio.Client.make ~https:(Some https_handler) net

(* Cap at 100 MiB. Real GitHub/GitLab/Slack responses are well under
   this; the bound just prevents a misbehaving server (or a redirect
   loop on a binary endpoint) from exhausting memory. *)
let max_body_size = 100 * 1024 * 1024

let read_body body =
  Eio.Buf_read.(of_flow ~max_size:max_body_size body |> take_all)

let get t ?(headers=Cohttp.Header.init ()) uri =
  Eio.Switch.run @@ fun sw ->
  let resp, body = Cohttp_eio.Client.get t ~sw ~headers uri in
  resp, read_body body

let post t ?(headers=Cohttp.Header.init ()) ?body uri =
  Eio.Switch.run @@ fun sw ->
  let body = Option.map Cohttp_eio.Body.of_string body in
  let resp, body = Cohttp_eio.Client.post t ~sw ~headers ?body uri in
  resp, read_body body

let patch t ?(headers=Cohttp.Header.init ()) ?body uri =
  Eio.Switch.run @@ fun sw ->
  let body = Option.map Cohttp_eio.Body.of_string body in
  let resp, body = Cohttp_eio.Client.patch t ~sw ~headers ?body uri in
  resp, read_body body
