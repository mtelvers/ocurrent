module Repo_id = Repo_id
module Api = Api
module Auth = Auth

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "gitlab"

  let webhook_events_total =
    let help = "Incoming webhook events" in
    Counter.v_label ~label_name:"event" ~help ~namespace ~subsystem "webhook_events_total"
end

(* Surprisingly this is a string compare, no signing involved.
   https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#secret-token
*)
let validate_webhook webhook_secret headers event =
  let request_header = Option.value ~default:"<empty>" @@ Cohttp.Header.get headers "X-Gitlab-Token" in
  if Eqaf.equal request_header webhook_secret then
    Ok ()
  else
    Error (Printf.sprintf {|
Invalid X-Gitlab-Token received for %s.
Please check the Webhook secrets are setup appropriately.
See https://docs.gitlab.com/ee/user/project/integrations/webhooks.html
|} event)

let webhook ~webhook_secret = object
    inherit Current_web.Resource.t

    method! post_raw _site req body =
      Log.info (fun f -> f "input_webhook: %a" Cohttp.Request.pp_hum req);
      let headers = Cohttp.Request.headers req in
      let event = Cohttp.Header.get headers "X-Gitlab-Event" in
      let event_str = Option.value ~default:"NONE" event in
      Log.info (fun f -> f "Got GitLab event %a" Fmt.(option ~none:(any "NONE") (quote string)) event);
      Prometheus.Counter.inc_one (Metrics.webhook_events_total event_str);
      let body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      match validate_webhook webhook_secret headers event_str with
      | Error msg ->
         Log.warn (fun f -> f "%s" msg);
         Current_web.Utils.Server.respond_string ~status:`Unauthorized ~body:"Invalid X-Gitlab-Token" ()
      | Ok () ->
         begin match event with
         | Some "Merge Request Hook" | Some "Push Hook" ->
           begin match Gitlab_types_j.webhook_of_string body with
            | `MergeRequest _ as x -> Api.input_webhook x
            | `Push _ as x -> Api.input_webhook x
           end
         | Some x -> Log.warn (fun f -> f "Unknown GitLab event type %S" x)
         | None -> Log.warn (fun f -> f "Missing GitLab event type in webhook!")
         end;
         Current_web.Utils.Server.respond_string ~status:`OK ~body:"OK" ()
  end
