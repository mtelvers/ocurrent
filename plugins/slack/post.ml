type t = {
  uri : Uri.t;
  http : Current_http.t;
}

let id = "slack-post"

module Key = Current.String
module Value = Current.String
module Outcome = Current.Unit

let publish t job _key message =
  Current.Job.start job ~level:Current.Level.Above_average;
  let headers = Cohttp.Header.init_with "Content-type" "application/json" in
  let body =
    `Assoc [ "text", `String message ]
    |> Yojson.to_string
  in
  let resp, _body = Current_http.post t.http ~headers ~body t.uri in
  match Cohttp.Response.status resp with
  | `OK -> Ok ()
  | err ->
    Fmt.error_msg "Slack post failed: %s" (Cohttp.Code.string_of_status err)

let pp f (key, value) = Fmt.pf f "Post %s: %s" key value

let auto_cancel = false
