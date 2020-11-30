open Aoc2020

let usage =
  Error.Bad_arg(Printf.sprintf
    "usage: %s N [ARGS]\n\
     \n\
     Where N is the calendar day and ARGS any required puzzle arguments." Sys.argv.(0))

let dispatch : string list -> string = function
  | x :: xs ->
    begin
      match x with
      | "1" ->  Day1.main xs
      | _ -> raise (Error.Bad_arg(Printf.sprintf "Unimplemented puzzle %s" x))
    end
    | _ -> raise usage

let () =
  try
    print_endline (dispatch (List.tl (Array.to_list Sys.argv)))
  with
  | Error.Bad_arg msg -> print_endline msg; exit 1
  | Sys_error msg -> print_endline msg; exit 1
