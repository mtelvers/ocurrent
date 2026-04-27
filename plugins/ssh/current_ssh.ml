open Current.Syntax

module R = Current_cache.Output(Run)

type t = { cache : R.t }

let create ~engine =
  { cache = R.create ~caps:(Current_cache.caps_of_engine engine) }

let run t ~schedule ~key host args =
  Current.component "ssh@,%s" host |>
  let> args = args in
  R.set t.cache ~schedule host key { Run.Value.args }
