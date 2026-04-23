module Server = struct
  include Cohttp_eio.Server

  (* Compatibility helpers for the cohttp-lwt-unix APIs the lib expects. *)

  let respond_redirect ?(headers=Cohttp.Header.init ()) ~uri () =
    let headers = Cohttp.Header.add headers "Location" (Uri.to_string uri) in
    respond_string ~headers ~status:`Found ~body:"" ()

  let respond_not_found () =
    respond_string ~status:`Not_found ~body:"Not found" ()

  let respond_error ?(status=`Internal_server_error) ~body () =
    respond_string ~status ~body ()
end

module Path = Cohttp.Path

let string_of_timestamp time =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
  Fmt.str "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

(* Add security headers to prevent clickjacking attacks *)
let add_security_headers headers =
  let headers = Cohttp.Header.add headers "X-Frame-Options" "DENY" in
  let headers = Cohttp.Header.add headers "Content-Security-Policy" "frame-ancestors 'none'" in
  headers
