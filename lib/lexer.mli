(** Location is the source location of lexed data. *)
module Location : sig
  type t = {
    line   :int;
    column :int;
  }

  val compare : t -> t -> int
  val string_of : t -> string

  (** Location zero value *)
  val zero : t
end

(** datum is lexed data and source location*)
type datum = {
  value    :string;
  location :Location.t;
}

(** Context wraps a character stream an provides the lexer with basic I/O operations *)
module Context : sig
  type t

  val of_char_stream : char Stream.t -> t
  val peek_char : t -> char option
  val save_char : t -> char -> unit
  val reset : t -> unit
  val datum : t -> datum
  val current_location : t -> Location.t
  val token_location : t -> Location.t
end

module Error : sig
  type id =
    | Bad_token
    | Unexpected_eof

  type t =
    | With_loc of id * Location.t

  val string_of_id : id -> string
  val string_of : t -> string

  exception Exn of t
end

val lex_single_char : Context.t -> char -> datum

val lex_until : Context.t -> (char -> bool) -> ?keep_last:bool -> char -> datum

val lex_until_or_eof : Context.t -> (char -> bool) -> ?keep_last:bool -> char -> datum
