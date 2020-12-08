module Location = struct
  type t = {
    line   :int;
    column :int;
  }

  let compare a b =
    if a < b then -1
    else if a > b then 1
    else 0

  let string_of loc = Printf.sprintf "line: %d column: %d" loc.line loc.column

  (* Most editors and compilers count lines from 1 but columns from 0. *)
  let zero = { line = 1; column = 0 }
end

type datum = {
  value    :string;
  location :Location.t;
}

module Context = struct
  type t = {
    stream                   :char Stream.t;
    buf                      :Buffer.t;
    mutable location_token   :Location.t;  (* Location at token reset *)
    mutable location_current :Location.t;  (* Continuously updated location *)
  }

  let of_char_stream stream =
    { stream
    ; buf              = Buffer.create 100
    ; location_token   = Location.zero
    ; location_current = Location.zero
    }

  let peek_char ctx = Stream.peek ctx.stream

  (* Save previously peeked character.

     Line and column information is updated based on character value.

     The character is added to the temporary lexer context buffer for possible
     later inclusion in a lexed token.
  *)
  let save_char ctx c =
    Buffer.add_char ctx.buf c;
    Stream.junk ctx.stream;
    let location = ctx.location_current in
    match c with
    | '\n' -> ctx.location_current <- { line = location.line + 1; column = 0 }
    | '\r' -> () (* Invisitble control char *)
    | '\t' -> ctx.location_current <- { location with column = location.column + 8 } (* Tabs are messy, count something. *)
    | _    -> ctx.location_current <- { location with column = location.column + 1 }

  (* Reset lexing state *)
  let reset ctx =
    Buffer.reset ctx.buf;
    ctx.location_token <- ctx.location_current

  (* Get lexed characters as a datum and reset token lexing state. *)
  let datum ctx =
    let datum = { value = Buffer.contents ctx.buf; location = ctx.location_token } in
    reset ctx; datum

  let current_location ctx = ctx.location_current
  let token_location ctx   = ctx.location_token
end

module Error = struct
  type id =
    | Bad_token
    | Unexpected_eof

  type t =
    | With_loc of id * Location.t

  let string_of_id = function
    | Bad_token      -> "Bad token"
    | Unexpected_eof -> "Unexpected EOF"

  let string_of = function
    | With_loc(id, loc) ->
      Printf.sprintf "%s: %s" (string_of_id id) (Location.string_of loc)

  exception Exn of t
end

(* Lex single character token. *)
let lex_single_char ctx c =
  Context.save_char ctx c;
  Context.datum ctx

(* Lex characters until predicate function says no. *)
let lex_until ctx pred ?(keep_last=false) c =
  Context.save_char ctx c;
  let rec consume () =
    match Context.peek_char ctx with
    | Some c ->
      if (pred c) then
        begin
          Context.save_char ctx c;
          consume ()
        end
      else
        begin
          (if keep_last then Context.save_char ctx c);
          Context.datum ctx
        end
    | None -> raise (Error.Exn(With_loc(Unexpected_eof, Context.token_location ctx)))
  in
  consume ()

(* Lex characters until predicate function says no or EOF occurs. *)
let lex_until_or_eof ctx pred ?(keep_last=false) c =
  try
    lex_until ctx pred ~keep_last c
  with
    Error.Exn(With_loc(Unexpected_eof, _)) -> Context.datum ctx
