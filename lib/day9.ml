(*
--- Day 9: Encoding Error ---

With your neighbor happily enjoying their video game, you turn your attention to
an open data port on the little screen in the seat in front of you.

Though the port is non-standard, you manage to connect it to your computer
through the clever use of several paperclips. Upon connection, the port outputs
a series of numbers (your puzzle input).

The data appears to be encrypted with the eXchange-Masking Addition System
(XMAS) which, conveniently for you, is an old cypher with an important weakness.

XMAS starts by transmitting a preamble of 25 numbers. After that, each number
you receive should be the sum of any two of the 25 immediately previous numbers.
The two numbers will have different values, and there might be more than one
such pair.

For example, suppose your preamble consists of the numbers 1 through 25 in a
random order. To be valid, the next number must be the sum of two of those
numbers:

    - 26 would be a valid next number, as it could be 1 plus 25 (or many other
      pairs, like 2 and 24).

    - 49 would be a valid next number, as it is the sum of 24 and 25.

    - 100 would not be valid; no two of the previous 25 numbers sum to 100.

    - 50 would also not be valid; although 25 appears in the previous 25
      numbers, the two numbers in the pair must be different.

Suppose the 26th number is 45, and the first number (no longer an option, as it
is more than 25 numbers ago) was 20. Now, for the next number to be valid, there
needs to be some pair of numbers among 1-19, 21-25, or 45 that add up to it:

    - 26 would still be a valid next number, as 1 and 25 are still within the
      previous 25 numbers.

    - 65 would not be valid, as no two of the available numbers sum to it.

    - 64 and 66 would both be valid, as they are the result of 19+45 and 21+45
      respectively.

Here is a larger example which only considers the previous 5 numbers (and has a
preamble of length 5):

35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576

In this example, after the 5-number preamble, almost every number is the sum of
two of the previous 5 numbers; the only number that does not follow this rule is
127.

The first step of attacking the weakness in the XMAS data is to find the first
number in the list (after the preamble) which is not the sum of two of the 25
numbers before it. What is the first number that does not have this property?

--- Part Two ---

The final step in breaking the XMAS encryption relies on the invalid number you
just found: you must find a contiguous set of at least two numbers in your list
which sum to the invalid number from step 1.

Again consider the above example:

35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576

In this list, adding up all of the numbers from 15 through 40 produces the
invalid number from step 1, 127. (Of course, the contiguous set of numbers in
your actual list might be much longer.)

To find the encryption weakness, add together the smallest and largest number in
this contiguous range; in this example, these are 15 and 47, producing 62.

What is the encryption weakness in your XMAS-encrypted list of numbers?
*)

(* Manages XMAS crypto sums.

   This is much harder than it needs to be.

   I tried to be clever to learn some OCaml and came up with this fancy rolling
   window for the sum updates. Consequently I spent 2 hours debugging this shit!
 *)
module Rolling_window = struct
  module Int64_map = Map.Make(Int64)

  exception Invariant_error

  type t = {
    dim             : int;
    mutable count   : int;
    num_vec         : int64 array;
    sum_vec         : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array2.t;
    mutable sum_map : int Int64_map.t;
  }

  let create dim =
    { dim = dim;
      count = 0;
      num_vec = Array.make dim 0L;
      sum_vec = Bigarray.Array2.create Int64 C_layout dim dim;
      sum_map = Int64_map.empty;
    }

  (* Nil sum sentinel value. Zero can be used as a nil value since it's illegal
     to sum two equal values so 0 + 0 is invalid. No other values in the input
     can sum to 0. *)
  let nil_sum = 0L

  (* next position to update in rolling window *)
  let window_pos t = t.count mod t.dim

  (* check if window is filled *)
  let is_window_filled t = t.count >= t.dim

  (* push number into rolling window and update internal sum states *)
  let push_number t number : unit =
    let column = window_pos t in
    let row = column in
    let sum a b = if a = b then nil_sum else Int64.add a b in
    let del_map_sums num_columns : unit =
      for i = 0 to (num_columns - 1) do
        let old_sum = t.sum_vec.{row, i} in
        if old_sum <> nil_sum then
          t.sum_map <-
            Int64_map.update old_sum (fun a ->
                match a with
                | Some(a) ->
                  if a < 1 then raise Invariant_error;
                  if a = 1 then None else Some(a-1)
                | None -> raise Invariant_error
              ) t.sum_map
      done
    in
    let add_map_sums num_columns : unit =
      for i = 0 to (num_columns - 1) do
        let new_sum = t.sum_vec.{row, i} in
        if new_sum <> nil_sum then
          t.sum_map <-
            Int64_map.update new_sum (fun a ->
                match a with
                | Some(a) ->
                  if a < 1 then raise Invariant_error;
                  Some(a+1)
                | None -> Some(1)
              ) t.sum_map
      done
    in
    let calc_row_sums () : unit =
      for i = 0 to (t.dim - 1) do
        t.sum_vec.{row, i} <- sum number t.num_vec.(i)
      done;
    in
    let num_del_columns = max 0 (min t.dim (t.count - (t.dim - 1))) in
    let num_add_columns = min 25 t.count in
    del_map_sums num_del_columns;
    t.num_vec.(column) <- number;
    calc_row_sums ();
    add_map_sums num_add_columns;
    t.count <- t.count + 1

  (* check if sum exists in window *)
  let sum_exists t sum =
      match Int64_map.find_opt sum t.sum_map with
      | Some(_) -> true
      | None -> false
end

let part1 (args :string list) :string =
  match args with
  | filename :: [] ->
    let input = File.lines filename in
    let window = Rolling_window.create 25 in
    (* search for non existing sum for number *)
    let rec search = function
      | x :: xs ->
        let number = (Int64.of_string x) in
        Rolling_window.push_number window number;
        if Rolling_window.is_window_filled window &&
           not (Rolling_window.sum_exists window number)
        then
          Some(number)
        else
          search xs
      | [] -> None
    in
    begin
      match search input with
      | Some(number) -> Int64.to_string number
      | None -> "Bad luck!"
    end
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))

let part2 (args :string list) :string =
  match args with
  | _filename :: [] -> "todo"
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))
