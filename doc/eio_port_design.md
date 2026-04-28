# OCurrent 2.0: Eio port design

Working notes from an April 2026 design conversation. Not final — this is the
shape we converged on, subject to revision once we do a trial plugin port.

## Context

- capnp-rpc 2.x (Thomas Leonard) is Eio-native. `capnp-rpc-lwt` exists as a
  deprecated transition shim.
- `current_rpc` is pinned at `capnp-rpc { < "2.0" }` (commit b3d0116) as a
  holding action — security fixes to 2.x won't reach us until we move.
- cohttp is on the same trajectory. cohttp-eio is primary; cohttp-lwt is in
  maintenance.
- Mark maintains all the significant downstream projects (ocaml-ci,
  docker-base-images, opam-repo-ci, mirage-ci, ocurrent-deployer), so
  cross-repo coordination is a scheduling problem, not a political one.

## Strategy: big-bang 2.0

- Ship `current.2.0.0` as a clean Eio break. No `ocurrent-lwt` transition shim.
- Users staying on Lwt pin `current { < "2.0" }` and stop there. The shim
  pattern doesn't pay rent for OCurrent the way it does for capnp-rpc:
  capnp-rpc is a tool embedded into Lwt apps; OCurrent *is* the app.
- ocluster 2.0 must land at roughly the same time — docker-base-images and
  ocaml-ci both depend on both.

## Package split

`current.term` promoted to its own opam package `current_term` (April 2026,
on master — non-Eio change). Library public_name went from `current.term`
to `current_term`. In-tree dune files updated. Downstream repos `ocaml-ci`
and `opam-repo-ci` each need a one-line `(libraries ... current.term ...)`
→ `current_term` change to keep building against current master.

- `lib_term/` depends only on `bos fmt current_incr` — no Lwt.
- Rationale: signals that the DSL is the stable foundation, the execution
  layer is what evolves. When the next scheduler shift happens, we don't
  want to break DSL versioning alongside it.

## Core signatures

### Engine

`Engine.create` is the one place that takes a whole env, once, at startup:

```ocaml
type 'a env = 'a constraint 'a = <
    clock       : float Eio.Time.clock;
    fs          : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : Eio.Process.mgr_ty Eio.Process.mgr;
    net         : _ Eio.Net.t;
    ..
  > as 'a

val create :
  env:_ env ->
  sw:Eio.Switch.t ->
  ?config:Config.t ->
  ?trace:(next:unit Eio.Promise.t -> results -> unit) ->
  (unit -> unit term) ->
  t
```

- Row-polymorphic env constraint (not `Eio_unix.Stdenv.base`) so tests can mock
  the subset that the engine actually uses. This is the capnp-rpc `Vat_config.cmd`
  pattern.
- `sw` is the engine's own switch — the one that scopes the pipeline run. Each
  job opens a child switch off it.

### Job

```ocaml
module Job : sig
  type t

  (* Accessors for capabilities stashed from the engine's env. *)
  val switch      : t -> Eio.Switch.t
  val clock       : t -> float Eio.Time.clock
  val fs          : t -> Eio.Fs.dir_ty Eio.Path.t
  val process_mgr : t -> Eio.Process.mgr_ty Eio.Process.mgr

  (* Existing API, but no more [Lwt.t] on anything. *)
  val start : ?timeout:Duration.t -> ?pool:unit Pool.t -> level:Level.t -> t -> unit
  val log : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val id : t -> job_id
  val on_cancel : t -> (string -> unit) -> unit
  (* ... *)
end
```

- `Job.switch` is explicit (not hidden) but plugin authors rarely touch it.
  Rationale: the engine owns the scope, plugins have no choice about which
  switch to attach to. Making it implicit via an accessor (rather than a `~sw`
  parameter) preserves today's mental model while staying idiomatic.
- This is a deliberate deviation from Thomas's "always explicit `~sw`"
  convention. Justified because we're the engine's closed ecosystem — the
  plugin author has no meaningful choice.

### BUILDER / PUBLISHER

```ocaml
module type BUILDER = sig
  type t
  val id : string
  module Key : WITH_DIGEST
  module Value : WITH_MARSHAL
  val build : t -> Current.Job.t -> Key.t -> Value.t Current.or_error
  val pp : Key.t Fmt.t
  val auto_cancel : bool
end
```

No `~sw` parameter. No `Lwt.t`. Direct-style, returns `_ or_error` synchronously.

### Plugin shape

A typical `current_docker` file becomes nearly line-identical to today:

```ocaml
let build auth job key =
  Current.Job.start job ~level:Current.Level.Mostly_harmless;
  let { Key.docker_context; tag; arch } = key in
  let* () = Auth.login ~docker_context ~job auth in
  ...
  let* () = Current.Process.exec ~cancellable:true ~job (Key.cmd key) in
  let* id = Current.Process.check_output ~cancellable:false ~job cmd in
  Current.Job.log job "Pulled %S -> %S" tag (String.trim id);
  Ok (Image.of_hash (String.trim id))
```

Changes vs. today:
- `open Lwt.Infix` → `open Current.Result.Syntax`.
- `>>=` → `let*`.
- `>|=` post-processing → plain `match` expressions.
- `Lwt_result.return x` → `Ok x`.
- Return type `_ or_error Lwt.t` → `_ or_error`.
- `Current.Process.exec` takes `string list` (idiomatic Eio), not
  `("", string array)` (`Lwt_process.command`).
- The plugin never sees `Eio.Switch.t` or `Eio.Process.mgr`.

## Trial port findings

Two trial ports done in April 2026 to stress-test the signatures.

### `plugins/ssh/run.ml` — trivial (30 lines, one-step publisher)

Three line changes: delete `open Lwt.Infix`, simplify `command` helper
(`("", [|...|])` → `string list`), replace `>>= fun () ->` with `;`. No
`let*` needed for this plugin. `current_ssh.ml` (the wrapper) unchanged
because the DSL doesn't change.

### `plugins/docker/build.ml` — moderate (120 lines, multi-step builder with pool/checkout/tmpdir/iidfile)

Mechanical diff. The `Lwt_result.Infix` chain in `with_context` becomes
`Current.Result.Syntax` with a `let*`. The `>|=` post-processing tunnel
(`match on Ok/Error; decrement prometheus counter`) becomes a plain match
followed by a sequence — easier to read. No capability threading, no `sw`
exposure, no Eio vocabulary above `Current.Process.exec`'s call sites.

### Signature decisions the trials confirmed

1. **`Current.Result.Syntax` lives in `current_term`.** Plugins need only
   one dep (`current`) to pick it up transitively, matching where
   `Current.Unit` and `Current.String` live.
2. **`Current.Process.exec` takes `string list`.** Confirmed cleaner at
   call sites. `Array.of_list` calls disappear from plugin code.
3. **`Current.Process.with_tmpdir : ?prefix:string -> (Fpath.t -> 'a) -> 'a`**
   — the CPS pattern survives; just drop `Lwt.t`. Polymorphic over the
   callback's return type (not locked to `_ or_error`).
4. **`Current_git.with_checkout`** — same shape as today, minus `Lwt.t`.
5. **`Current.Job.start : ?timeout -> ?pool -> level -> t -> unit`** —
   direct-style sync signature. May suspend under the hood (confirmation
   gate, pool acquisition) but callers don't see that.
6. **`?pool:unit Current.Pool.t`** argument threads through `Job.start`
   unchanged. The Eio semaphore/pool underneath is invisible to plugins.

### Bonus wins surfaced by the trials

1. **`Fun.protect` for Prometheus inc/dec pairs.** Today's `>|=` chain
   silently skips `dec_one` if the process raises. Direct style makes
   `Fun.protect ~finally:(...)` the obvious idiom — one line, exception-safe.
   Same improvement applies to `docker/pull.ml`, `docker/push.ml`,
   `docker/push_manifest.ml`.
2. **Dead code surfaces.** `docker/build.ml` has an unused `use_pool`
   function. Full plugin audit during the port will probably find a few
   more.
3. **`>|=` tunnels become plain matches.** Nested `Stdlib.Result.map`
   inside `>|=` inside `>|=` is one of the less-readable patterns in
   today's codebase. Direct style flattens it.

### Untested, to verify during real port

- `Pool.of_fn` custom pool creation (ocluster, `current_git` ssh-agent
  handling).
- `Job.on_cancel` / `with_handler` cleanup callbacks — maps to
  `Eio.Switch.on_release`, but verify the fire-on-cancel-only vs
  fire-on-any-end semantics match today's behaviour.
- Parallel fibers inside a plugin — none of the surveyed plugins do this
  today.

## Layering audit

Who actually sees `Eio.Switch.t`:

| Layer | Sees `sw`? |
|---|---|
| `Current.Engine`, `Current.Job` | yes — core |
| `Current.Process`, `Current_git.with_checkout`, a handful of helpers | yes — primitive-spawning |
| `current_docker/*`, `current_github/*`, user pipelines | no — all via `Process.exec ~job` |

Roughly 3-5 files in the whole codebase reach into `Job.switch`. Everything
else uses `Current.Process.exec ~job`.

## Eio idiom notes (from capnp-rpc 2.1.1)

- `~sw` is a named parameter when callers own the scope. Never stashed in a
  record for public APIs.
- `env` is never passed whole below top level. Pull out `net`, `fs`, etc.
  with row polymorphism (`_ Eio.Net.t`, not `Eio.Net.t`).
- Capabilities are stashed at construction (`Vat_config.create ~net`), not
  threaded through every call.
- Multi-capability APIs use row-polymorphic object constraints, not
  `Stdenv.base`.

## Sibling work: `prometheus-eio`

Spike an Eio fork of `mirage/prometheus` as part of the 2.0 work, rather than
vendoring the text formatter or bridging via `Lwt.state` tricks.

### Why a fork is small

Measurements of `prometheus-app` v1.3 (April 2026):

| File | Lines | Lwt touchpoints | What's Lwt-shaped |
|---|---|---|---|
| `src/prometheus.ml` | 405 | 11 | `metrics_lwt`/`pre_collect_lwt` fields, `register_lwt`/`register_pre_collect_lwt`, Lwt_list iteration in `collect`, 5 `track_inprogress`/`time` wrappers |
| `app/prometheus_app.ml` | 165 | 1 | `Cohttp` functor (~18 lines at the bottom) |
| `app/prometheus_unix.ml` | 108 | 2 | `Cohttp_lwt_unix` server setup (~10 lines) |

Total Lwt-touching code across 678 source lines: ~50-70 lines. Everything
else — metric name/label machinery, Counter/Gauge/Summary/Histogram impls,
`TextFormat_0_0_4`, `Runtime` GC metrics — is already sync.

### Fork plan

1. `CollectorRegistry.collect` becomes synchronous. Delete `Lwt_list.*` and
   `>>=`, iterate synchronously. ~10 lines simpler.
2. Drop `register_lwt` and `register_pre_collect_lwt`. Anyone needing an
   async collector can register a sync one that reads a ref.
3. Rewrite `track_inprogress`/`time` as sync one-liners over `Fun.protect`.
   Sync signatures.
4. Replace `Prometheus_app.Cohttp` functor with a cohttp-eio callback.
   ~20 lines.
5. Replace `Prometheus_unix` with cmdliner + cohttp-eio server. ~30 lines.

Tests convert from `alcotest-lwt` to plain `alcotest` — all collection is
sync now.

Estimate: one focused day, including tests. The codebase shrinks.

### Upstream alignment

Thomas's v1.2 CHANGES note on the Lwt collector API:

> Note that this is a temporary feature while we wait for OCaml 5 to be
> released, when this can be replaced by the use of effects.

An Eio fork executes upstream's stated direction. Best shape:

- Build `prometheus-eio` as a standalone package (new repo or
  mirage/prometheus branch).
- OCurrent 2.0 depends on `prometheus-eio` instead of `prometheus` +
  `prometheus-app`. Drops `lwt`, `cohttp-lwt`, `cohttp-lwt-unix` from the
  2.0 dep tree entirely.
- Engage with Thomas about landing as a sibling package (capnp-rpc
  pattern: `capnp-rpc` + `capnp-rpc-net` + `capnp-rpc-unix`) or replacing
  the Lwt version in a prometheus 2.0 release. The PR might become
  prometheus 2.0.

## Risks

### Real

1. **ocluster server-side co-migration.** The scheduler daemon and the
   `current_ocluster` plugin both use capnp-rpc — they have to move with
   OCurrent 2.0. Budget both together.
2. **Blocking IO inside the engine.** Log writes (`open_out_gen;
   output_string; close_out`) and synchronous SQLite queries block the
   scheduler. Today Lwt tolerates it; under Eio it stalls the whole fiber.
   Fixes: `Eio.Path.save` for logs, `Eio_unix.run_in_systhread` for the
   SQLite hot path.

### Non-risks

- `Current_incr` is scheduler-agnostic — no change.
- `lib_term` compiles without Lwt today — no change.
- `Lwt_dllist`, `Lwt_condition`, `Lwt.wait`/`wakeup` — direct Eio equivalents
  with the same semantics. Mechanical swap.
- **Windows.** OCurrent itself runs Linux-side; the only Windows-relevant
  piece in the ecosystem is `ocluster-worker`, which doesn't depend on
  OCurrent. The worker can stay on Lwt + capnp-rpc 1.x indefinitely, since
  capnp-rpc's wire protocol is schema-defined and a Lwt-1.x worker can talk
  to an Eio-2.x scheduler over the wire unchanged. `eio_windows` parity
  becomes a concern for the eventual ocluster-worker port, on its own
  timeline — not for OCurrent 2.0.

## Open verification tasks

1. ~~Prometheus story~~ — resolved. Plan: fork as `prometheus-eio` (see
   "Sibling work" above). One day of work. Not blocking.
2. ~~Trial-port a small plugin~~ — resolved. `ssh/run.ml` and
   `docker/build.ml` both port cleanly (see "Trial port findings").
3. ~~Windows parity~~ — resolved. OCurrent itself doesn't run on Windows;
   `ocluster-worker` is the Windows product and doesn't depend on OCurrent.
   Worker port is a separate project on a separate timeline. No gating
   concern for OCurrent 2.0.

**All three green. No outstanding gating concerns before scheduling.**

## Unresolved

- Pool API shape — keep `unit Pool.t` with `of_fn` constructor (today's
  shape, backed by `Eio.Semaphore`), or expose `Eio.Pool.t 'a` directly?
  Trials used the passthrough form and didn't force a decision. Real port
  of ocluster / `current_git` ssh-agent pooling will resolve.
- `Current.Process.exec` ergonomics for single-arg commands. `string list`
  is idiomatic but `Cmd.docker [...]` helpers in plugins return lists
  today, so no change needed. Flag if the trial finds plugins constructing
  commands differently.

---

## Implementation notes (2026)

This section records where the final implementation diverged from the
design above. Read the rest of the doc first for context; this is the
delta.

### Engine_env removed; `~sw ~env` go to `Engine.create` directly

The original sketch had a process-wide `Current.Engine_env` module that
stashed `~sw ~env` for `Job`, `Process`, and the cache to read back.
That was a global. The final shape passes them to `Engine.create`
explicitly:

```ocaml
val create :
  sw:Eio.Switch.t ->
  env:_ env ->
  ?clock:_ ->
  ?config:Config.t ->
  ?trace:_ ->
  (t -> unit term) ->
  t
```

Per-engine state lives on `Engine.t`. The factory thunk receives the
engine so plugins built inside can derive a `Current_cache.caps`
record from it. There's no longer any global init step.

### Per-instance cache (no `Current_cache.Runtime` stash)

The cache used to keep `caps` in a global ref initialised by an engine
hook. This too was removed. `Current_cache.Make(B)`, `.Output(P)`, and
`.Generic(Op)` now each expose:

```ocaml
type t
val create : caps:caps -> t
val get/set : t -> ?schedule:_ -> Op.t -> Op.Key.t -> _ Current.Primitive.t
```

`Current_cache.caps_of_engine engine` produces the `caps` record.
Plugins call this once inside the engine factory and pass it to each
of their cache modules' `create`.

### Plugins use `make`/`default` returning a first-class module

Several iterations on the plugin shape:

1. `create ~engine` returning a record (worked but coupled plugins to
   the engine type).
2. `create ~caps ~net …` returning a record (decoupled, used by
   slack/git/github/gitlab/ssh).
3. `Make () (E)` generative functor (used briefly for docker and
   ocluster, ergonomically heavier).
4. **Current shape** for plugins that want module-level cache scope
   (docker, ocluster): a plain function returning a first-class
   module:

   ```ocaml
   val Current_docker.make :
     caps:Current_cache.caps ->
     git:Current_git.t ->
     docker_context:string option ->
     (module Current_docker.S.DOCKER with type Image.t = Image.t)

   val Current_ocluster.make :
     caps:Current_cache.caps ->
     connection:Connection.t ->
     (module Current_ocluster.S)
   ```

   Callers unpack with `let module Docker = (val …) in`. Each call
   returns a fresh module identity (and thus fresh caches), the same
   semantics that a generative functor gives but with less ceremony.

Plugins without a cycle between their `Op.t` and outer `t`
(slack/git/github/gitlab/ssh) keep the simpler `create ~caps`
returning a record.

### `current_ocluster` — hybrid `t` + `make`

`current_ocluster` had the cycle problem: the build operation's `Op.t`
was the same record as the plugin's outer `t`, which itself contained
`Build.t`. Resolved by:

- `Make () (E : { caps; connection })` (later `make ~caps ~connection`)
  builds the cache at module scope — outside `t`.
- The inner `t` shrinks to per-call defaults (timeout/push_auth/
  secrets/urgent/cache_hint/level) with `with_*` helpers, plus `v ()`.
- `Op.t = t` no longer cycles through `Build.t`, because `Build.t`
  isn't in `t` any more — it's the module-scope `build_cache`.

### Connection-style shared state takes its own `~sw ~clock`

`Current_ocluster.Connection.create ~sw ~clock` takes both: `~sw`
scopes the reconnection daemon, `~clock` is needed for backoff sleeps.
Anonymous GitHub access (`Current_github.Api.Anonymous.create ~sw ~net
~clock`) follows the same pattern — it doesn't go through the cache
machinery, so it doesn't need a `caps`, just the raw capabilities.

### Job log file uses `Unix.openfile`/`Unix.write_substring`

Plain Unix file APIs, not `Eio.Path.open_out`. Reason: `Eio.Path`'s
open yields on the linux/io_uring backend, and the yield in
`Job.create`/`Job.start` raced with `current_cache.ml`'s `t.job_id`
assignment. Synchronous Unix I/O sidesteps the issue; writes are short
lines that hit the page cache in microseconds, not a meaningful
scheduler stall in practice.

`with_tmpdir` does use `Eio.Path` (`mkdir`/`rmtree`) — it now takes
`~job` so it can pull `fs` from the job's stashed capabilities. SQLite
stays synchronous.

### Engine.update via signal counter, not a one-shot promise

The original mutex+option+ref dance for `next_evaluation`/`update` was
replaced with a monotonic counter. Each iteration snapshots the
counter as a baseline; a bridge fiber resolves the trace's `~next`
promise once the counter advances past the baseline. That gives
coalescing for free (the engine doesn't care how many updates landed
between iterations) and naturally drops updates issued before the
engine's first iteration (e.g. `SVar.set` during pipeline init).

### Switch_ext lifts the spawn-managed-switch pattern

`Monitor` (one switch per activation) and `Current_cache` instance
slots both fork a daemon that runs `Switch.run sw → resolve sw_p →
await release_p`. That's `Current.Switch_ext.spawn_managed
~parent_sw`, returning `(Switch.t, unit Promise.u)`.

### Pool.of_fn forwards `~register_cancel`

The user-supplied `get` function for `Pool.of_fn` now receives
`~register_cancel`. Implementations that handle cancellation through
`Switch.on_release sw` (like ocluster's `Connection.submit`) can
ignore it. The previous behaviour silently dropped the registration.
