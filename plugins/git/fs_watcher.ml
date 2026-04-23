(* Replacement for irmin-watcher, which is Lwt-only. Uses inotify-eio to
   watch a directory and invoke a callback with the leaf filename whenever
   an event of interest occurs. *)

let events = [
  Inotify.S_Close_write;
  Inotify.S_Moved_to;
  Inotify.S_Create;
  Inotify.S_Delete;
]

(* Start watching [dir] and return an [unwatch] function. [on_change name]
   is called on the caller's behalf inside a daemon fiber, once per event,
   with the relative filename (or "" for directory-level events). *)
let hook ~sw dir on_change =
  let t = Eio_inotify.create () in
  let _watch = Eio_inotify.add_watch t dir events in
  let stop = Atomic.make false in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      if Atomic.get stop then `Stop_daemon
      else
        match Eio_inotify.read t with
        | (_, _, _, Some name) ->
          (try on_change name with _ -> ());
          loop ()
        | (_, _, _, None) ->
          (* Event applies to the directory itself rather than an entry;
             notify with an empty string so callers can decide what to do. *)
          (try on_change "" with _ -> ());
          loop ()
        | exception Unix.Unix_error (Unix.EBADF, _, _) ->
          (* Descriptor closed by [unwatch]; clean exit. *)
          `Stop_daemon
    in
    loop ()
  );
  fun () ->
    Atomic.set stop true;
    Eio_inotify.close t
