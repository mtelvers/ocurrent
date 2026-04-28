### Writing plugins

To create your own primitive operations (such as `Docker.build`), you'll probably want to use the
[Current_cache](https://ocurrent.github.io/ocurrent/current/Current_cache/index.html) library.
This handles all the details of starting builds, recording the results, managing log files, etc.

A minimal example looks something like this:

```ocaml
# #require "current,current.cache";;

# open Current.Syntax;;

# module Frob = struct
    type t = No_context

    module Key = Current.String
    module Value = Current.String

    let id = "frob"

    let build No_context job _key =
      (* Wait in a queue or wait for resources here if needed. *)
      Current.Job.start job ~level:Current.Level.Harmless;
      (* Do the work here. *)
      Ok "frobbed"

    let pp f key = Fmt.pf f "frob %a" Key.pp key

    let auto_cancel = false
  end;;
...
# module FC = Current_cache.Make(Frob);;
...
```

`Current_cache.Make(Frob)` produces a module with a per-engine cache type. Each
engine instantiates a cache once at startup and reuses it for every call:

```ocaml
let frob fc key =
  Current.component "Frob" |>
  let> key = key in
  FC.get fc Frob.No_context key
```

The plugin user supplies `fc` from their engine factory. The shape (with
your own `Eio_main.run` and `Eio.Switch.run` outside) is roughly:

```text
Current.Engine.create ~sw ~env ~config (fun engine ->
  let caps = Current_cache.caps_of_engine engine in
  let fc = FC.create ~caps in
  pipeline ~fc ())
```

`Current_cache.caps_of_engine engine` produces a `Current_cache.caps` record
containing the engine's switch and Eio capabilities. Pass that to each
`Make`/`Output`/`Generic` cache's `create ~caps`.

If your plugin wraps several Op modules and you'd rather expose a single
"plugin instance" rather than a record of caches, follow the docker pattern
— a `make` function that returns a first-class module, with the caches as
let-bindings inside:

```text
let make ~caps =
  (module struct
    module FC = Current_cache.Make(Frob)
    let frob_cache = FC.create ~caps
    let frob key =
      Current.component "Frob" |>
      let> key = key in
      FC.get frob_cache Frob.No_context key
  end : sig
    val frob : Frob.Key.t Current.t -> Frob.Value.t Current.t
  end)
```

Callers unpack with `let module Plugin = (val make ~caps) in …`.

---

The `frob` function is the one exposed to users.
These functions always start by getting the actual values
from their `Current.t` arguments using `let>` (and `and>`, for additional arguments).
The `Current.component` line provides the label for the dot diagrams.

The `build` function does the real work.
It should try to build the output value from the key and return it.
If you start any operations that need to be cancelled, use `Job.on_cancel` to register a callback.
`job` can be used for logging.

If you need extra context that shouldn't be part of the key
(e.g. ssh keys used to connect to a remote machine)
then you can use the `t` type for that, replacing `No_context` with whatever you require.
Note that asking for the same key with two different contexts will *not* perform two different builds.

The `Key` module describes the type of the inputs.
It needs to provide a `digest` function for turning keys into unique strings.
For short strings, you can just use the string itself (this is what `Current.String` does).
For longer data, you might want to generate a SHA hash.
For complex data types, you might want to convert to JSON, etc.

The `Value` module describes the outputs.
You need to provide `marshal` and `unmarshal` functions for storing the results on disk.
Note that for large values you can write the data elsewhere and just use its identifier as the value.
For example, the value returned by `Docker.build` is just the image's tag.
The image itself is stored by Docker.

`auto_cancel` says whether OCurrent should cancel the job automatically if the build is no longer
needed. For example, if you're building a Git commit and then the user makes another commit, you can decide
whether it would be better to continue with the old build (while also building the new commit), or abort it.

`level` gives a rough estimate of the cost or risk of the operation.
You can use the `--confirm` option to decide which operations OCurrent will perform automatically.

As well as caching build operations (which take a key and produce a value), you can also cache outputs
(which take a key and a value as input). For example, the `Docker.tag` operation above uses the output cache.

See the [Current_cache](https://ocurrent.github.io/ocurrent/current/Current_cache/index.html) API for more information.
