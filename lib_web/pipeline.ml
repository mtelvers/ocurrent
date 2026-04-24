let render_svg ctx a =
  let uri = Context.uri ctx in
  let env = Uri.query uri |> List.filter_map (function
      | (_, []) -> None
      | (k, v :: _) -> Some (k, v)
    ) in
  let old_query = Uri.query uri in
  let collapse_link ~k ~v =
      let query = (k, [v]) :: List.remove_assoc k old_query in
      Some (Uri.make ~path:"/" ~query () |> Uri.to_string)
  and job_info { Current.Metadata.job_id; update } =
    let url = job_id |> Option.map (fun id -> Fmt.str "/job/%s" id) in
    update, url
  in
  let dotfile = Fmt.to_to_string (Current.Analysis.pp_dot ~env ~collapse_link ~job_info) a in
  Eio.Switch.run @@ fun sw ->
  let mgr = Current.Engine_env.process_mgr () in
  let stdin_r, stdin_w = Eio.Process.pipe ~sw mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let proc =
    Eio.Process.spawn ~sw mgr
      ~stdin:stdin_r
      ~stdout:stdout_w
      ~stderr:stdout_w
      ["dot"; "-Tsvg"]
  in
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  let svg = ref "" in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string dotfile stdin_w;
      Eio.Flow.close stdin_w)
    (fun () ->
      svg := Eio.Buf_read.(of_flow ~max_size:(100 * 1024 * 1024) stdout_r |> take_all));
  match Eio.Process.await proc with
  | `Exited 0 -> Ok !svg
  | `Exited i -> Fmt.error_msg "dot failed (exit status %d) - is graphviz installed?" i
  | `Signaled i -> Fmt.error_msg "dot crashed (signal %d)" i

let r ~engine = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    match render_svg ctx (Current.Engine.pipeline engine) with
    | Ok body ->
      let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
      Utils.Server.respond_string ~status:`OK ~headers ~body ()
    | Error (`Msg msg) ->
      Context.respond_error ctx `Internal_server_error msg
end
