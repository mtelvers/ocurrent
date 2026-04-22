(* The core term language. *)

module S = S

module Output = Output

module Make (Metadata : sig type t end) : sig
  include S.TERM with
    type metadata := Metadata.t and
    type 'a primitive = ('a Output.t * Metadata.t option) Current_incr.t

  module Analysis : S.ANALYSIS with
    type 'a term := 'a t and
    type metadata := Metadata.t

  module Executor : S.EXECUTOR with
    type 'a term := 'a t
end

(** Direct-style error handling for plugin authors. Open
    [Current.Result.Syntax] to use [let*], [let+] etc. on OCaml's
    [result] type. *)
module Result : sig
  module Syntax : sig
    val (let*) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
    val (let+) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
    val (and*) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
    val (and+) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
  end
end
