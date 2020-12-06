open Aoc2020

let usage =
  Error.Bad_arg(Printf.sprintf
    "usage: %s PUZZLE [ARGS]\n\
     \n\
     Where PUZZLE is the puzzle name and ARGS any required puzzle arguments." Sys.argv.(0))

let dispatch : string list -> string = function
  | x :: xs ->
    begin
      match x with
      | "1.part1" ->  Day1.part1 xs
      | "1.part2" ->  Day1.part2 xs
      | "2.part1" ->  Day2.part1 xs
      | "2.part2" ->  Day2.part2 xs
      | "3.part1" ->  Day3.part1 xs
      | "3.part2" ->  Day3.part2 xs
      | "4.part1" ->  Day4.part1 xs
      | "4.part2" ->  Day4.part2 xs
      | "5.part1" ->  Day5.part1 xs
      | "5.part2" ->  Day5.part2 xs
      | "6.part1" ->  Day6.part1 xs
      | "6.part2" ->  Day6.part2 xs
      | _ -> raise (Error.Bad_arg(Printf.sprintf "Unimplemented puzzle %s" x))
    end
    | _ -> raise usage

let () =
  try
    print_endline (dispatch (List.tl (Array.to_list Sys.argv)))
  with
  | Error.Bad_arg msg -> print_endline msg; exit 1
  | Sys_error msg -> print_endline msg; exit 1
