open Current.Syntax

module S = S

let pp_tag = Fmt.using (Astring.String.cuts ~sep:":") Fmt.(list ~sep:(any ":@,") string)

module Raw = struct
  module Image = Image

  module PullC = Current_cache.Make(Pull)
  module PeekC = Current_cache.Make(Peek)
  module BC = Current_cache.Make(Build)
  module RC = Current_cache.Make(Run)
  module PrC = Current_cache.Make(Pread)
  module TC = Current_cache.Output(Tag)
  module Push_cache = Current_cache.Output(Push)
  module SC = Current_cache.Output(Service)
  module CC = Current_cache.Output(Compose)
  module CCC = Current_cache.Output(Compose_cli)
  module MC = Current_cache.Output(Push_manifest)

  type t = {
    pull_cache : PullC.t;
    peek_cache : PeekC.t;
    build_cache : BC.t;
    run_cache : RC.t;
    pread_cache : PrC.t;
    tag_cache : TC.t;
    push_cache : Push_cache.t;
    service_cache : SC.t;
    compose_cache : CC.t;
    compose_cli_cache : CCC.t;
    push_manifest_cache : MC.t;
    git : Current_git.t;
  }

  let create ~engine ~git =
    let caps = Current_cache.caps_of_engine engine in
    {
      pull_cache = PullC.create ~caps;
      peek_cache = PeekC.create ~caps;
      build_cache = BC.create ~caps;
      run_cache = RC.create ~caps;
      pread_cache = PrC.create ~caps;
      tag_cache = TC.create ~caps;
      push_cache = Push_cache.create ~caps;
      service_cache = SC.create ~caps;
      compose_cache = CC.create ~caps;
      compose_cli_cache = CCC.create ~caps;
      push_manifest_cache = MC.create ~caps;
      git;
    }

  let pull t ~docker_context ~schedule ?auth ?server ?arch tag =
    PullC.get t.pull_cache ~schedule (Auth.v ~auth ~server) { Pull.Key.docker_context; tag; arch }

  let peek t ~docker_context ~schedule ~arch tag =
    PeekC.get t.peek_cache ~schedule Peek.No_context { Peek.Key.docker_context; tag; arch }

  let build t ~docker_context ?level ?schedule ?timeout ?(squash=false) ?(buildx = false) ?dockerfile ?path ?pool ?(build_args=[]) ~pull commit =
    let dockerfile =
      match dockerfile with
      | None -> `File (Fpath.v "Dockerfile")
      | Some (`File _ as f) -> f
      | Some (`Contents c) -> `Contents c
    in
    BC.get t.build_cache ?schedule { Build.pull; pool; timeout; level; git = t.git }
    { Build.Key.commit; dockerfile; docker_context; squash; buildx; build_args; path }

  let run t ~docker_context ?pool ?(run_args=[]) image ~args  =
    RC.get t.run_cache { Run.pool } { Run.Key.image; args; docker_context; run_args }

  let pread t ~docker_context ?pool ?(run_args=[]) image ~args =
    PrC.get t.pread_cache { Pread.pool } { Pread.Key.image; args; docker_context; run_args }

  let tag t ~docker_context ~tag image =
    TC.set t.tag_cache Tag.No_context { Tag.Key.tag; docker_context } { Tag.Value.image }

  let push t ~docker_context ?auth ?server ~tag image =
    Push_cache.set t.push_cache (Auth.v ~auth ~server) { Push.Key.tag; docker_context } { Push.Value.image }

  let service t ~docker_context ~name ~image () =
    SC.set t.service_cache Service.No_context { Service.Key.name; docker_context } { Service.Value.image }

  let compose t ?(pull=true) ~docker_context ~name ~contents () =
    CC.set t.compose_cache Compose.{ pull } { Compose.Key.name; docker_context } { Compose.Value.contents }

  let compose_cli t ?(pull=true) ?(up_args = []) ~docker_context ~name ~detach ~contents () =
     CCC.set t.compose_cli_cache Compose_cli.{ pull } { Compose_cli.Key.name; docker_context; detach ; up_args } { Compose_cli.Value.contents }

  let push_manifest t ?auth ?server ~tag manifests =
    MC.set t.push_manifest_cache (Auth.v ~auth ~server) tag { Push_manifest.Value.manifests }

  module Cmd = struct
    open Current.Result.Syntax

    type t = string list

    let docker args ~docker_context = Cmd.docker ~docker_context args

    let rm_f id = docker ["container"; "rm"; "-f"; id]
    let kill id = docker ["container"; "kill"; id]

    (* Try to "docker kill $id". If it fails, just log a warning and continue. *)
    let try_kill_container ~docker_context ~job id =
      match Current.Process.exec ~cancellable:false ~job (kill ~docker_context id) with
      | Ok () -> ()
      | Error (`Msg m) -> Current.Job.log job "Warning: Failed to kill container %S: %s" id m

    let with_container ~docker_context ~kill_on_cancel ~job t fn =
      let* id = Current.Process.check_output ~cancellable:false ~job t in
      let id = String.trim id in
      let did_rm = ref false in
      let result =
        try
          if kill_on_cancel then
            Current.Job.on_cancel job (fun _ ->
              if not !did_rm then try_kill_container ~docker_context ~job id);
          fn id
        with ex ->
          Fmt.error_msg "with_container: uncaught exception: %a" Fmt.exn ex
      in
      did_rm := true;
      match Current.Process.exec ~cancellable:false ~job (rm_f ~docker_context id) with
      | Ok () -> result         (* (the common case, where removing the container succeeds) *)
      | Error (`Msg rm_error) as rm_e ->
        match result with
        | Ok _ -> rm_e
        | Error _ as e ->
          (* The job failed, and removing the container failed too.
             Log the second error and return the first. *)
          Current.Job.log job "Failed to remove container %S when job failed: %s" id rm_error;
          e

    let pp = Cmd.pp
  end
end

module Make (Host : S.HOST) = struct
  module Image = Image

  type nonrec t = Raw.t

  let create ~engine ~git = Raw.create ~engine ~git

  let docker_context = Host.docker_context

  let pp_opt_arch f = function
    | None -> ()
    | Some arch -> Fmt.pf f "@,%s" arch

  let pull t ?auth ?server ?label ?arch ~schedule tag =
    let label = Option.value label ~default:tag in
    Current.component "pull %s%a" label pp_opt_arch arch |>
    let> () = Current.return () in
    Raw.pull t ~docker_context ~schedule ?arch ?auth ?server tag

  let peek t ?label ~arch ~schedule tag =
    let label = Option.value label ~default:tag in
    Current.component "peek %s@,%s" label arch |>
    let> () = Current.return () in
    Raw.peek t ~docker_context ~schedule ~arch tag

  let pp_sp_label = Fmt.(option (sp ++ string))

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit
    | `Dir path -> Current.map (fun path -> `Dir path) path

  let build t ?level ?schedule ?timeout ?squash ?buildx ?label ?dockerfile ?path ?pool ?build_args ~pull src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> dockerfile = Current.option_seq dockerfile in
    Raw.build t ~docker_context ?level ?schedule ?timeout ?squash ?buildx ?dockerfile ?path ?pool ?build_args ~pull commit

  let run t ?label ?pool ?run_args image ~args  =
    Current.component "run%a" pp_sp_label label |>
    let> image = image in
    Raw.run t ~docker_context ?pool ?run_args image ~args

  let pread t ?label ?pool ?run_args image ~args  =
    Current.component "pread%a" pp_sp_label label |>
    let> image = image in
    Raw.pread t ~docker_context ?pool ?run_args image ~args

  let tag t ~tag image =
    Current.component "docker-tag@,%a" pp_tag tag |>
    let> image = image in
    Raw.tag t ~docker_context ~tag image

  let push t ?auth ?server ~tag image =
    Current.component "docker-push@,%a" pp_tag tag |>
    let> image = image in
    Raw.push t ~docker_context ?auth ?server ~tag image

  let service t ~name ~image () =
    Current.component "docker-service@,%s" name |>
    let> image = image in
    Raw.service t ~docker_context ~name ~image ()

  let compose t ?pull ~name ~contents () =
    Current.component "docker-compose@,%s" name |>
    let> contents = contents in
    Raw.compose t ?pull ~docker_context ~name ~contents ()

  let compose_cli t ?pull ?up_args ~name ~detach ~contents () =
    Current.component "docker-compose-cli@,%s" name |>
    let> contents = contents in
    Raw.compose_cli t ?pull ?up_args ~docker_context ~name ~detach ~contents ()

  let push_manifest t ?auth ?server ~tag manifests =
    Current.component "docker-push-manifest@,%a" pp_tag tag |>
    let> manifests = Current.list_seq manifests in
    Raw.push_manifest t ?auth ?server ~tag manifests
end

module Default = Make(struct
    let docker_context = Sys.getenv_opt "DOCKER_CONTEXT"
  end)
