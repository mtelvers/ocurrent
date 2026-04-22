val pp_cmd : Format.formatter -> string list -> unit

val exec :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> string list -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  ?env:string array ->
  cancellable:bool -> job:Job.t -> string list ->
  unit Current_term.S.or_error

val check_output :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> string list -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> string list ->
  string Current_term.S.or_error

val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a) -> 'a
