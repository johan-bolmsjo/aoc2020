(*
--- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation at
a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold
coins used there have a little picture of a starfish; the locals just call them
stars. None of the currency exchanges seem to have heard of them, but somehow,
you'll need to find fifty of these coins by the time you arrive so you can pay
the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense
report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then
multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to
2020; what do you get if you multiply them together?

--- Part Two ---

The Elves in accounting are thankful for your help; one of them even offers you
a starfish coin they had left over from a past vacation. They offer you a second
one if you can find three numbers in your expense report that meet the same
criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366,
and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?
*)

let slurp_lines filename =
  File.lines filename |>
  List.map int_of_string

let target_sum = 2020

let part1 (args :string list) :string =
  match args with
  | filename :: [] ->
    let lines = slurp_lines filename in
    let result = List.find_map (fun a ->
        let b = target_sum - a in
        if List.exists (fun a -> a = b) lines then
          Some(a, b)
        else
          None
      ) lines
    in
    begin
      match result with
      | Some (a, b) -> string_of_int (a * b)
      | None -> ""
    end
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))

let part2 (args :string list) :string =
  match args with
  | filename :: [] ->
    let lines = slurp_lines filename |> List.sort compare in
    let max_id = 2 in
    let list_product l = (List.fold_left ( * ) 1 l) in
    let rec search (acc :int) (id :int) (l :int list) :int list option =
      match l with
      | x :: xs ->
        let sum = acc + x in
        if sum <= target_sum then
          if id < max_id then
            (* Go deeper *)
            match search sum (id + 1) xs with
            | Some l -> Some(x :: l)
            | None -> search acc id xs (* Next number on same level *)
          else
            begin (* Last level *)
              if sum == target_sum then
                Some [x]
              else
                (* Next number on last level *)
                search acc id xs
            end
        else
          None
      | [] -> None
    in
    begin
      match (search 0 0 lines) with
      | Some l -> string_of_int (list_product l)
      | None -> ""
    end
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))
