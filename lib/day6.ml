(*
--- Day 6: Custom Customs ---

As your flight approaches the regional airport where you'll switch to a much
larger plane, customs declaration forms
(https://en.wikipedia.org/wiki/Customs_declaration) are distributed to the
passengers.

The form asks a series of 26 yes-or-no questions marked a through z. All you
need to do is identify the questions for which anyone in your group answers
"yes". Since your group is just you, this doesn't take very long.

However, the person sitting next to you seems to be experiencing a language
barrier and asks if you can help. For each of the people in their group, you
write down the questions for which they answer "yes", one per line. For example:

abcx
abcy
abcz

In this group, there are 6 questions to which anyone answered "yes": a, b, c, x,
y, and z. (Duplicate answers to the same question don't count extra; each
question counts at most once.)

Another group asks for your help, then another, and eventually you've collected
answers from every group on the plane (your puzzle input). Each group's answers
are separated by a blank line, and within each group, each person's answers are
on a single line. For example:

abc

a
b
c

ab
ac

a
a
a
a

b

This list represents answers from five groups:

    - The first group contains one person who answered "yes" to 3 questions: a,
      b, and c.
    - The second group contains three people; combined, they answered "yes" to 3
      questions: a, b, and c.
    - The third group contains two people; combined, they answered "yes" to 3
      questions: a, b, and c.
    - The fourth group contains four people; combined, they answered "yes" to
      only 1 question, a.
    - The last group contains one person who answered "yes" to only 1 question, b.

In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.

For each group, count the number of questions to which anyone answered "yes".
What is the sum of those counts?

--- Part Two ---

As you finish the last group's customs declaration, you notice that you misread
one word in the instructions:

You don't need to identify the questions to which anyone answered "yes"; you
need to identify the questions to which everyone answered "yes"!

Using the same example as above:

abc

a
b
c

ab
ac

a
a
a
a

b

This list represents answers from five groups:

    - In the first group, everyone (all 1 person) answered "yes" to 3 questions:
      a, b, and c.
    - In the second group, there is no question to which everyone answered "yes".
    - In the third group, everyone answered yes to only 1 question, a. Since
      some people did not answer "yes" to b or c, they don't count.
    - In the fourth group, everyone answered yes to only 1 question, a.
    - In the fifth group, everyone (all 1 person) answered "yes" to 1 question, b.

In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.

For each group, count the number of questions to which everyone answered "yes".
What is the sum of those counts?
*)

module Char_set = Set.Make(Char)

(* Read one groups answers from [input], returns the groups answers and the
   remaining input. *)
let read_group_answers input :(Char_set.t list * string list) =
  let rec aux (group_answers, input) =
    match input with
    | line :: remaining_lines ->
      if line = "" then
        (group_answers, remaining_lines)
      else
        let member_answers =
          String.to_seq line |> Seq.fold_left (fun set char ->
              Char_set.add char set
            ) Char_set.empty
        in
        aux (member_answers :: group_answers, remaining_lines)
    | [] -> (group_answers, input)
  in
  aux ([], input)

(* Count number of yes answers from group members using the set operation
   [setop], for example Char_set.inter or Char_set.union *)
let group_yes_answers setup = function
  | member_answers :: remaining_group_answers ->
    List.fold_left (fun acc member_answers ->
        setup acc member_answers
      ) member_answers remaining_group_answers |> Char_set.cardinal
  | [] -> 0

(* Parse input and count all groups yes answers. The set operation
   [group_member_setop], for example Char_set.inter or Char_set.union, is used
   to calculate each groups contribution to the total score. *)
let parse_input group_member_setop input =
  let rec aux acc = function
    | [] -> acc
    | _ as input ->
      let (group_answers, input) = read_group_answers input in
      aux (acc + (group_yes_answers group_member_setop group_answers)) input
  in
  aux 0 input

let part1 (args :string list) :string =
  match args with
  | filename :: [] ->
    File.lines filename |>
    parse_input Char_set.union |>
    string_of_int
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))

let part2 (args :string list) :string =
  match args with
  | filename :: [] ->
    File.lines filename |>
    parse_input Char_set.inter |>
    string_of_int
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))
