open Current.Syntax

module R = Current_cache.Output(Run)

type t = { cache : R.t }

let create ~caps =
  { cache = R.create ~caps }

let run t ~schedule ~key host args =
  Current.component "ssh@,%s" host |>
  let> args = args in
  R.set t.cache ~schedule host key { Run.Value.args }
