(* HTTPS client helper.  Wraps cohttp-eio + tls-eio with a small API that
   mirrors the cohttp-lwt-unix call surface, so the github plugin's call
   sites stay close to their original shape. *)

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

let client () =
  let env = Current.Engine_env.get_env () in
  let net = Eio.Stdenv.net env in
  Cohttp_eio.Client.make ~https:(Some https_handler) net

let read_body body =
  Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all)

let get ?(headers=Cohttp.Header.init ()) uri =
  Eio.Switch.run @@ fun sw ->
  let resp, body = Cohttp_eio.Client.get (client ()) ~sw ~headers uri in
  resp, read_body body

let post ?(headers=Cohttp.Header.init ()) ?body uri =
  Eio.Switch.run @@ fun sw ->
  let body = Option.map Cohttp_eio.Body.of_string body in
  let resp, body = Cohttp_eio.Client.post (client ()) ~sw ~headers ?body uri in
  resp, read_body body
