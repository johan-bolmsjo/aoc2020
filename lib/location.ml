type t = {
  line   :int;
  column :int;
}

let compare a b =
  if a < b then -1
  else if a > b then 1
  else 0

let string_of loc =
  Printf.sprintf "line: %d column: %d" loc.line loc.column

(* Most editors and compilers (that I use) count lines from 1 but columns from 0. *)
let zero = { line = 1; column = 0 }
