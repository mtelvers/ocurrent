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

val with_tmpdir : job:Job.t -> ?prefix:string -> (Fpath.t -> 'a) -> 'a
(** [with_tmpdir ~job ?prefix fn] creates a temporary directory under
    {!Filename.get_temp_dir_name}, runs [fn] with its path, and removes it
    on return. The directory is created and removed via [Eio.Path] using
    the [fs] capability on [job]. *)
