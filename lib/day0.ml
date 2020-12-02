(*
*)

let part1 (args :string list) :string =
  match args with
  | _filename :: [] -> "todo"
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))

let part2 (args :string list) :string =
  match args with
  | _filename :: [] -> "todo"
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))
