(* JSON adapter matching GitLab's webhook envelope, which tags events with
   an "object_kind" field rather than the standard atdgen tagged-variant
   encoding. *)
module Webhook_event = Atdgen_runtime.Json_adapter.Type_field.Make (struct
  let type_field_name = "object_kind"
end)
