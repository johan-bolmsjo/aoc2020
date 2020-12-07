(** Location information for lexer and parser errors. *)

type t = {
  line   :int;
  column :int;
}

val compare : t -> t -> int

val string_of : t -> string

(* Location zero value *)
val zero : t
