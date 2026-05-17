# Migrating to OCurrent 2.0

OCurrent 2.0 is an Eio port. The pipeline DSL surface — `let>`,
`Current.component`, `Current.list_iter`, the `let*`/`let+` syntax — is
unchanged, but the runtime plumbing is different in ways that affect:

- the dependency tree (no more Lwt where OCurrent itself was using it),
- how you wire up an engine (Eio top-level, switch-scoped),
- how plugins are written (no `Lwt.t` returns from `BUILDER`/`PUBLISHER`),
- a few signature renames in `Job` and `Current.Process`.

This document is a step-by-step migration guide derived from porting a
real downstream consumer (`ocurrent-deployer`).

## Prerequisites

1. **OCaml 5.0 or later.** Eio requires effects, which arrived in OCaml
   5.0. Bump the floor in your `dune-project` if you had OCaml 4.x.
2. **A recent `opam-repository`.** OCurrent 2.0 depends on capnp-rpc
   2.x and mirage-crypto 1.x; older repository snapshots will fail to
   resolve. Pull the latest before starting.
3. **A `prometheus-eio` source.** Until the Eio rewrite of
   `mirage/prometheus` lands upstream, OCurrent depends on a fork at
   `https://github.com/mtelvers/prometheus.git#eio`.

## Setting up the switch

A 4.x → 5.x bump is large enough that the cleanest path is a fresh
local switch. From your project root:

```sh
opam switch create . 5.4.1 --no-install
opam pin add -yn prometheus.dev      https://github.com/mtelvers/prometheus.git#eio
opam pin add -yn prometheus-app.dev  https://github.com/mtelvers/prometheus.git#eio
opam pin add -yn current.dev         https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_term.dev    https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_web.dev     https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_git.dev     https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_github.dev  https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_gitlab.dev  https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_docker.dev  https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_slack.dev   https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_ssh.dev     https://github.com/ocurrent/ocurrent.git#eio
opam pin add -yn current_rpc.dev     https://github.com/ocurrent/ocurrent.git#eio
```

If your project uses OCluster, also pin the matching ocluster fork
(see [Migrating consumers of `current_ocluster`](#migrating-consumers-of-current_ocluster)
below):

```sh
opam pin add -yn ocluster-api-eio.dev https://github.com/ocurrent/ocluster.git#eio
opam pin add -yn current_ocluster.dev https://github.com/ocurrent/ocluster.git#eio
```

`-n` (`--no-action`) suppresses the install at pin time so opam can
resolve the full graph in one pass when you run:

```sh
opam install -y . --deps-only
```

If you're iterating on a local checkout of OCurrent or OCluster, pin
to the working tree directly so opam picks up uncommitted edits:

```sh
opam pin add -yn current.dev /path/to/ocurrent --kind=path
```

Or, with `git+file://` pins, pass `--working-dir` to `opam install` to
include uncommitted changes — by default it uses the last commit only.

## Verifying your opam metadata

After the install resolves, check that your project's `dune-project`
`(depends ...)` matches what got installed. The fastest way is:

```sh
day10 build --log
```

The log lines of the form `"<pkg>" -> { ... }` tell you the *actual*
direct dependencies of each package as opam computed them. Anything in
that set that's missing from your `(depends ...)` is a hole that will
only bite a downstream user with a fresh switch.

The rule: declare every library mentioned in any `(libraries ...)` in
your dune files. Don't try to be clever about transitive — multiple
packages declaring the same dep is fine, opam deduplicates anyway.

## Code changes

The OCurrent 2.0 changes downstream code falls into a handful of
patterns. Most of your edits will be one of these.

### Entry point: `Eio_main.run` ladder

The pre-2.0 entry point was `Lwt_main.run` running `Lwt.choose
[Current.Engine.thread; Current_web.run; …]`. The engine is no longer
a thread you manage explicitly: `Engine.create` forks a daemon onto
the switch you give it, so your main only needs to keep that switch
open.

The minimal 2.0 ladder is:

```ocaml
let main () config mode prometheus_config =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  …
  let engine =
    Current.Engine.create ~sw ~env ~config (fun engine ->
      let caps = Current_cache.caps_of_engine engine in
      (* Build plugin instances here from [caps]. *)
      pipeline …)
  in
  let site = Current_web.Site.v ~has_role ~name:"…" routes in
  Eio.Fiber.all
    (Current_web.run ~net ~mode site :: Prometheus_unix.serve ~net prometheus_config)
```

The order matters:

- `Eio_main.run` is at the top.
- `Eio.Switch.run` opens the switch the engine lives on.
- `Current.Engine.create ~sw ~env` takes the switch and Eio env
  explicitly. The factory thunk receives the new `Engine.t` so plugin
  instances built inside can call `Current_cache.caps_of_engine` to
  derive a `caps` record (used to instantiate caches — see *Plugin
  instance creation* below).
- `Current_web.run` and `Prometheus_unix.serve` both return fiber
  bodies (`unit -> unit` and `(unit -> unit) list` respectively) rather
  than blocking. Compose them with `Eio.Fiber.all`. Each owns its
  listening socket via an internal switch; cancelling a fiber shuts
  that server down. Exiting `Eio.Fiber.all` then exits `Switch.run`,
  which tears down the engine daemon.
- `Current_web.run` now takes `~net` explicitly (was implicit via
  global state in pre-2.0 betas).

> **Note for early-2.0-beta users**: an earlier design had a global
> `Current.Engine_env.init ~sw ~env` call that stashed capabilities for
> lower modules to retrieve. That module has been removed — `~sw` and
> `~env` go directly to `Engine.create`, and per-engine state lives on
> `Engine.t`. If you ported during the beta, delete the
> `Engine_env.init` line and add `~sw ~env` to your `Engine.create`
> call.

### Plugin instance creation

Pre-2.0 plugins were typically used as global modules:

```ocaml
module Docker = Current_docker.Default
module Github = Current_github
…
Docker.build src ~pull:false dockerfile
```

In 2.0, plugins that own caches need a per-engine instance. The
construction happens inside the engine factory, where `engine` is in
scope. Pattern:

```ocaml
Current.Engine.create ~sw ~env ~config (fun engine ->
  let caps = Current_cache.caps_of_engine engine in
  let git = Current_git.create ~caps in
  let module Docker = (val Current_docker.default ~caps ~git) in
  let github = Current_github.Api.create ~caps ~net github_config in
  …
  pipeline ~git ~docker:(module Docker) ~github ())
```

The shapes per plugin:

- **`Current_git.create ~caps`** returns a `Current_git.t` (record of
  cache instances). Pass it to `Current_git.fetch`, etc.
- **`Current_slack.create ~caps ~net`** returns a `Current_slack.t`.
- **`Current_github.Api.create ~caps ~net config`** and
  **`Current_github.App.create ~caps ~net config`**.
- **`Current_gitlab.Api.create ~caps ~net config`**.
- **`Current_ssh.create ~caps`**.
- **`Current_docker.make ~caps ~git ~docker_context`** returns a
  first-class module of type `(module Current_docker.S.DOCKER)`.
  Unpack with `let module Docker = (val …) in`. There's also
  `Current_docker.default ~caps ~git`, which reads `$DOCKER_CONTEXT`
  from the environment.
- **`Current_ocluster.make ~caps ~connection`** returns a first-class
  module of type `(module Current_ocluster.S)`.

If you need a plugin in a function defined outside the factory,
either:

1. Take it as an argument (record or first-class module), threaded
   from the factory; or
2. Construct everything inline inside the factory thunk.

Don't try to construct plugin instances above the `Engine.create`
call — `caps` doesn't exist yet.

### Capnp-rpc 2.x: `client_only_vat` and `serve`

capnp-rpc 2.x is Eio-native. The thunk-style constructors are gone.

```ocaml
(* before *)
let vat = Capnp_rpc_unix.client_only_vat () in

(* after *)
let vat = Capnp_rpc_unix.client_only_vat ~sw net in
```

```ocaml
(* before *)
Capnp_rpc_unix.serve capnp ~restore >>= fun vat -> …

(* after *)
let vat = Capnp_rpc_unix.serve ~sw ~restore capnp in
…
```

`Vat_config.cmd` now takes `env`:

```ocaml
let cmd env =
  let info = Cmd.info "rpc_server" in
  Cmd.v info Term.(term_result (const main $ Capnp_rpc_unix.Vat_config.cmd env $ …))

let () =
  Eio_main.run @@ fun env ->
  exit @@ Cmd.eval (cmd env)
```

Watch for `Term.env` shadowing if you `open Cmdliner` — rename your
parameter to `eio_env` or qualify.

### Builder/Publisher signatures: drop `Lwt.t`

`Op.build`, `Op.publish`, `Op.run` used to return `… Current.or_error
Lwt.t`. They now return `… Current.or_error` directly.

```ocaml
(* before *)
let publish No_context job key value =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  …
  Current.Process.exec ~cancellable:true ~job cmd >>!= fun () ->
  Lwt.return @@ Ok ()

(* after *)
let publish No_context job key value =
  Current.Job.start job ~level:Current.Level.Dangerous;
  …
  let* () = Current.Process.exec ~cancellable:true ~job cmd in
  Ok ()
```

A `let*` for `Result.bind` is the idiomatic short-circuit:

```ocaml
let ( let* ) = Result.bind
```

### `Current.Job.start` is unit-returning

Was `Current.Job.start job ~level:_ : unit Lwt.t`. Now `unit`. The
`>>= fun () ->` after every call disappears — replace with `;`.

### `Current.Process.exec` takes a `string list`

The Lwt-era signature was `Current.Process.exec : ("", string array) ->
… Lwt.t`. The 2.0 signature takes a plain `string list`:

```ocaml
(* before *)
let cmd = ("", [| "ssh"; ssh_host; "mirage-redeploy"; name |]) in
Current.Process.exec ~cancellable:true ~job cmd

(* after *)
let cmd = ["ssh"; ssh_host; "mirage-redeploy"; name] in
Current.Process.exec ~cancellable:true ~job cmd
```

`Array.of_list` calls disappear from plugin code as a result.

### `Current.Switch` is gone — use `Eio.Switch`

The internal `Current.Switch` abstraction has been removed.
`Job.create` now takes `~sw:Eio.Switch.t`. Plugins that opened a switch
to scope a job's lifetime use `Eio.Switch.run` directly:

```ocaml
(* before *)
let switch = Current.Switch.create ~label:"clone" () in
Fun.protect
  ~finally:(fun () -> Current.Switch.turn_off switch)
  (fun () ->
    let job = Job.create ~switch ~label ~config () in
    …)

(* after *)
Eio.Switch.run @@ fun sw ->
let job = Job.create ~sw ~label ~config () in
…
```

`Job.switch : t -> Eio.Switch.t` is exposed too — use it to fork
fibers or spawn subprocesses scoped to the job.

### `Current.Engine.thread` is gone

The engine runs as an Eio daemon on the switch passed to
`Engine.create ~sw`. There is no thread to compose with `Lwt.choose`.
The web server and Prometheus server now return fiber bodies; compose
them with `Eio.Fiber.all`:

```ocaml
(* before *)
Lwt.choose [
  Current.Engine.thread engine;
  Current_web.run ~mode site;
  Prometheus_unix.serve config;
]

(* after *)
Eio.Fiber.all
  (Current_web.run ~net ~mode site :: Prometheus_unix.serve ~net config)
```

### `Lwt.finalize` → `Fun.protect`

Direct mechanical translation:

```ocaml
(* before *)
Lwt.finalize
  (fun () -> work ())
  (fun () -> cleanup (); Lwt.return_unit)

(* after *)
Fun.protect
  (fun () -> work ())
  ~finally:(fun () -> cleanup ())
```

### `Logging.run` (and similar wrappers) take a thunk

If you have a helper that wraps `Lwt_main.run` and adds error logging
(common pattern), it now takes a thunk:

```ocaml
(* before *)
val run : unit Current.or_error Lwt.t -> unit Current.or_error
let run x =
  match Lwt_main.run x with
  | Ok () -> Ok ()
  | Error _ as e -> log_it e; e

(* after *)
val run : (unit -> unit Current.or_error) -> unit Current.or_error
let run f =
  match f () with
  | Ok () -> Ok ()
  | Error _ as e -> log_it e; e
```

Call sites:

```ocaml
(* before *)
Logging.run begin
  Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ]
end

(* after *)
Logging.run @@ fun () ->
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let net = Eio.Stdenv.net env in
let engine = Current.Engine.create ~sw ~env ~config (fun engine -> …) in
…
Current_web.run ~net ~mode site ()
```

### `dune` files

Drop `lwt`, `lwt.unix`, `alcotest-lwt`. Add `eio`, `eio_main` (in the
executable's libs). The library that contains plugin definitions
generally needs `eio` for `Eio.Switch`/`Eio.Promise`/`Eio.Condition`
references; the executable additionally needs `eio_main` to call
`Eio_main.run`.

### Tests using Alcotest

If your tests used `alcotest-lwt`, drop the wrapper:

```ocaml
(* before *)
let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "my-suite"
       [ ("group", Test_group.tests); ]

(* in tests/test_group.ml *)
let tests = [
  Alcotest_lwt.test_case_sync "case" `Quick test_fn;
]

(* after *)
let () =
  Alcotest.run "my-suite"
    [ ("group", Test_group.tests); ]

(* in tests/test_group.ml *)
let tests = [
  Alcotest.test_case "case" `Quick test_fn;
]
```

Drop `alcotest-lwt` from the test's `(libraries ...)` in `dune`. If
some tests genuinely need to drive Eio code, run each from a fresh
`Eio_main.run` inside the test body — Alcotest's plain `test_case`
gives you a synchronous callback, no special Eio runner needed.

### Migrating consumers of `current_ocluster`

`ocluster-api` (the Lwt schema bindings used by the OCluster scheduler
and worker binaries) hasn't moved to capnp-rpc 2.x — those binaries
stay on Lwt for now. For OCurrent 2.0, OCluster ships a parallel
package `ocluster-api-eio` built against capnp-rpc 2.x, and
`current_ocluster` itself is the Eio plugin built on top of it. The
wire protocol is identical, so a Lwt 1.x worker can talk to a 2.x
client unchanged.

Pin both:

```sh
opam pin add -yn ocluster-api-eio.dev https://github.com/ocurrent/ocluster.git#eio
opam pin add -yn current_ocluster.dev https://github.com/ocurrent/ocluster.git#eio
```

Three changes to your code:

1. **`Connection.create` now takes `~sw ~clock`.** The connection
   forks a reconnection daemon on this switch and uses the clock for
   backoff sleeps:

   ```ocaml
   (* before *)
   let sched = Current_ocluster.Connection.create
       (Capnp_rpc_unix.Vat.import_exn vat sched) in

   (* after *)
   let sched = Current_ocluster.Connection.create
       ~sw ~clock                                       (* new *)
       (Capnp_rpc_unix.Vat.import_exn vat sched) in
   ```

2. **The plugin is a first-class module, not a global value.** Build
   one inside the engine factory:

   ```ocaml
   Current.Engine.create ~sw ~env ~config (fun engine ->
     let caps = Current_cache.caps_of_engine engine in
     let module Cluster = (val Current_ocluster.make ~caps ~connection:sched) in
     let cluster_t = Cluster.v ?push_auth () in
     …
     Cluster.build cluster_t ~pool ~src ~options dockerfile)
   ```

   The inner `Cluster.t` carries the per-call defaults
   (timeout/push_auth/secrets/urgent/cache_hint/level), tweaked via
   `Cluster.with_timeout`/`with_push_auth`/`with_secrets`/`with_urgent`.
   The build cache is held inside the module instance; calling
   `make` again yields a fresh cache.

3. **`Cluster_api.*` references in your own code.** The library is
   now `cluster_api_eio`; module name is `Cluster_api_eio`. For each
   file that uses `Cluster_api.Docker.Spec`, etc., add a module alias
   at the top:

   ```ocaml
   module Cluster_api = Current_ocluster.Cluster_api
   ```

   `Current_ocluster` re-exports `Cluster_api_eio` under the
   familiar `Cluster_api` name, so the rest of the file is unchanged.

### `wait_for_log_data` and other Lwt → Eio cookbook

| Lwt | Eio |
|---|---|
| `Lwt.wait () / Lwt.wakeup` | `Eio.Promise.create () / Eio.Promise.resolve` |
| `Lwt.async (fun () -> body)` | `Eio.Fiber.fork ~sw (fun () -> body)` for finite work, `Eio.Fiber.fork_daemon ~sw` for an infinite loop |
| `Lwt_condition.t` | `Eio.Condition.t` (also requires an `Eio.Mutex.t`) |
| `Lwt_unix.sleep d` | `Eio.Time.sleep clock d` (`clock` from `Eio.Stdenv.clock env`) |
| `Lwt.pause ()` | `Eio.Fiber.yield ()` |
| `Lwt.choose [a; b]` | `Eio.Fiber.first` (returns the first to complete and cancels the other) or `Eio.Fiber.both` (runs both, returns when both finish) |
| `Lwt_list.iter_s` | plain `List.iter` |
| `Lwt_list.iter_p` | `Eio.Fiber.List.iter` |
| `Lwt_io` (file I/O) | `Eio.Path` + `Eio.Buf_read` (always with a `~max_size:`!) |

## Common errors

### `Library "tls-eio" not found`

You're on an OCurrent commit before
`d570f4f current.opam: declare lib_http's external libraries`.
Update your `current.dev` pin (`opam update`, then
`opam install -y current.dev`) and re-resolve.

### `Switch finished!`

Something is calling into Eio with a switch that's already exited.
The most common cause is forking a fiber that outlives a `Switch.run`
scope. The engine's switch is the one passed to
`Current.Engine.create ~sw`; anything you fork onto it must live no
longer than the surrounding `Switch.run`. Building plugin instances
above `Engine.create` (where `caps.sw` would be referencing a switch
that's already on its way out) is a typical cause — construct them
inside the factory thunk instead.

### `Cmdliner.Term.env` shadowed our env

Inside a `Term.(...)` block, `Cmdliner.Term.env` shadows your `env`
parameter. Rename to `eio_env` (or fully qualify). Affects the
`Vat_config.cmd env` site in particular.

### `This expression has type unit but an expression was expected of type 'a Lwt.t`

You're working on a file you haven't ported yet — the surrounding
code is still in Lwt. Look for the closest `>>= fun () ->` or
`Lwt.return` and remove the Lwt-isms.

### `Library "current_ocluster" not found`

You need to pin OCluster's `eio` branch alongside OCurrent. See
[Migrating consumers of `current_ocluster`](#migrating-consumers-of-current_ocluster).

### `Unbound module Cluster_api`

The library was renamed to `cluster_api_eio`. Add at the top of the
file:

```ocaml
module Cluster_api = Current_ocluster.Cluster_api
```

### `Error: Library "ocluster-api" not found` (when pinning local checkout)

Your `current_ocluster.opam` file is stale: it was generated from an
older `dune-project` that pinned `ocluster-api`. After editing
`dune-project`, run `dune build` in the OCluster checkout to
regenerate the `.opam` files, then commit them. Without that, opam
sees an opam metadata file that contradicts your dune-project.

If you want opam to pick up uncommitted edits in a `git+file://` pin,
pass `--working-dir` to `opam install`.

## Verification

After everything builds:

1. The web UI comes up and the pipeline page renders.
2. A job log streams to completion in the browser.
3. RPC endpoints respond if you use `current_rpc`.
4. Restart the service and confirm jobs persist correctly.
