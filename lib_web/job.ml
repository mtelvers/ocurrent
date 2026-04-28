open Tyxml.Html
open Astring

let sep = "@@LOG@@"

let max_log_chunk_size = 102400L  (* 100K at a time *)

(* Each web request that reads a log chunk runs the open / seek / read
   on a sys-thread so the Eio scheduler isn't blocked while the kernel
   is doing the disk read. *)
let read ~start path =
  Eio_unix.run_in_systhread @@ fun () ->
  let ch = open_in_bin (Fpath.to_string path) in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let len = LargeFile.in_channel_length ch in
  let (+) = Int64.add in
  let (-) = Int64.sub in
  let start = if start < 0L then len + start else start in
  let start = if start < 0L then 0L else if start > len then len else start in
  LargeFile.seek_in ch start;
  let len = min max_log_chunk_size (len - start) in
  really_input_string ch (Int64.to_int len), start + len

(* Streaming source for a job log response: first the template
   header, then the log itself (waiting for more data if the job is
   still running), then the template footer. Mirrors the original
   Lwt_stream-based body used under cohttp-lwt. *)
module Log_source = struct
  type state = {
    pre : string;
    mutable pre_offset : int;
    post : string;
    mutable post_offset : int;
    path : Fpath.t;
    job_id : string;
    ansi : Ansi.t;
    mutable log_offset : int64;
    mutable pending : string;
    mutable pending_offset : int;
    mutable phase : [`Pre | `Log | `Post | `Done];
  }

  let blit_from src_str src_off dst dst_off n =
    Cstruct.blit_from_string src_str src_off dst dst_off n

  let rec single_read t dst =
    match t.phase with
    | `Done -> raise End_of_file
    | `Pre ->
      let avail = String.length t.pre - t.pre_offset in
      if avail = 0 then (t.phase <- `Log; single_read t dst)
      else (
        let len = min (Cstruct.length dst) avail in
        blit_from t.pre t.pre_offset dst 0 len;
        t.pre_offset <- t.pre_offset + len;
        len
      )
    | `Log ->
      let avail = String.length t.pending - t.pending_offset in
      if avail > 0 then (
        let len = min (Cstruct.length dst) avail in
        blit_from t.pending t.pending_offset dst 0 len;
        t.pending_offset <- t.pending_offset + len;
        len
      ) else (
        match read ~start:t.log_offset t.path with
        | "", _ ->
          (match Current.Job.lookup_running t.job_id with
           | None -> t.phase <- `Post; single_read t dst
           | Some job ->
             Current.Job.wait_for_log_data job;
             single_read t dst)
        | data, next ->
          t.pending <- Ansi.process t.ansi data;
          t.pending_offset <- 0;
          t.log_offset <- next;
          single_read t dst
      )
    | `Post ->
      let avail = String.length t.post - t.post_offset in
      if avail = 0 then (t.phase <- `Done; raise End_of_file)
      else (
        let len = min (Cstruct.length dst) avail in
        blit_from t.post t.post_offset dst 0 len;
        t.post_offset <- t.post_offset + len;
        len
      )

  let read_methods = []

  let create ~pre ~post ~path ~job_id ~ansi =
    let state = {
      pre; pre_offset = 0;
      post; post_offset = 0;
      path; job_id; ansi;
      log_offset = 0L;
      pending = ""; pending_offset = 0;
      phase = `Pre;
    } in
    let ops = Eio.Flow.Pi.source (module struct
      type nonrec t = state
      let single_read = single_read
      let read_methods = read_methods
    end) in
    Eio.Resource.T (state, ops)
end

(* Build an Eio flow source that streams the log response body:
   the template header, then the log content (polling for more while the
   job is still running), then the template footer. *)
let log_body_source ctx ~engine ~actions ~job_id ~log:path =
  let ansi = Ansi.create () in
  let action op = a_action (Fmt.str "/job/%s/%s" job_id op) in
  let csrf = Context.csrf ctx in
  let rebuild_button =
    if actions#rebuild = None then []
    else
      [form ~a:[action "rebuild"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Rebuild"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] () ]
      ]
  in
  let cancel_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.cancelled_state job = Ok () ->
      [form ~a:[action "cancel"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Cancel"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] () ]
      ]
    | _ -> []
  in
  let start_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.is_waiting_for_confirmation job ->
      [form ~a:[action "start"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Start now"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] () ]
      ]
    | _ -> []
  in
  let job_item ~label id =
    let label = txt label in
    if id = job_id then b [label]
    else a ~a:[a_href (Fmt.str "/job/%s" id)] [label]
  in
  let history =
    let registry = Current.Engine.cache_registry engine in
    match Current_cache.Db.history ~registry ~limit:10 ~job_id with
    | None, [] -> []
    | current, past ->
      let items = past |> List.map (fun entry ->
          let label = Int64.to_string entry.Current_cache.Db.build in
          let item = job_item ~label entry.job_id in
          li [item]
        ) in
      let items =
        match current with
        | None -> items
        | Some id -> li [job_item id ~label:"(building)"] :: items
      in
      [div ~a:[a_class ["build-history"]]
         [txt "Build: ";
          ol items]
      ]
  in
  let line_numbers_js = [script ~a:[a_src (Xml.uri_of_string "/js/line-numbers.js")] (txt "");]
  in
  let tmpl =
    Context.template ctx (
      line_numbers_js @
      history @
      rebuild_button @
      cancel_button @
      start_button @
      [pre [txt sep]]
    )
  in
  match String.cut ~sep tmpl with
  | None -> assert false
  | Some (pre, post) ->
    Log_source.create ~pre ~post ~path ~job_id ~ansi

type actions = <
  rebuild : (unit -> string) option;
>

let lookup_actions ~engine job_id =
  let state = Current.Engine.state engine in
  let jobs = state.Current.Engine.jobs in
  match Current.Job.Map.find_opt job_id jobs with
  | Some a -> (a :> actions)
  | None ->
    object
      method rebuild = None
    end

let job ~engine ~job_id = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let actions = lookup_actions ~engine job_id in
    match Current.Job.log_path job_id with
    | Error (`Msg msg) -> Context.respond_error ctx `Bad_request msg
    | Ok path ->
      let body = log_body_source ctx ~engine ~actions ~job_id ~log:path in
      let headers =
        (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
        Cohttp.Header.init_with "X-Accel-Buffering" "no"
        |> Utils.add_security_headers
      in
      Utils.Server.respond ~status:`OK ~headers ~body ()
end

let rebuild ~engine ~job_id = object
  inherit Resource.t

  val! can_post = `Builder

  method! private post ctx  _body =
    let actions = lookup_actions ~engine job_id in
    match actions#rebuild with
    | None -> Context.respond_error ctx `Bad_request "Job does not support rebuild"
    | Some rebuild ->
      let new_id = rebuild () in
      ignore ctx;
      Utils.Server.respond_redirect ~uri:(Uri.of_string ("/job/" ^ new_id)) ()
end

let cancel ~job_id = object
  inherit Resource.t

  val! can_post = `Builder

  method! private post ctx _body =
    match Current.Job.lookup_running job_id with
    | None -> Context.respond_error ctx `Bad_request "Job does not support cancel (already finished?)"
    | Some job ->
      Current.Job.cancel job "Cancelled by user";
      Context.respond_redirect ctx (Uri.of_string "/")
end

let start ~job_id = object
  inherit Resource.t

  val! can_post = `Admin

  method! private post ctx _body =
    match Current.Job.lookup_running job_id with
    | None -> Context.respond_error ctx `Bad_request "Job is not awaiting confirmation"
    | Some j ->
      Current.Job.approve_early_start j;
      let id = Current.Job.id j in
      Context.respond_redirect ctx (Uri.of_string ("/job/" ^ id))
end

let id ~date ~log = Fmt.str "%s/%s" date log

let routes ~engine = Routes.[
    s "job" / str / str /? nil @--> (fun date log -> job ~engine ~job_id:(id ~date ~log));
    s "job" / str / str / s "rebuild" /? nil @--> (fun date log -> rebuild ~engine ~job_id:(id ~date ~log));
    s "job" / str / str / s "cancel" /? nil @--> (fun date log -> cancel ~job_id:(id ~date ~log));
    s "job" / str / str / s "start" /? nil @--> (fun date log -> start ~job_id:(id ~date ~log));
  ]
