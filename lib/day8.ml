(*
--- Day 8: Handheld Halting ---

Your flight to the major airline hub reaches cruising altitude without incident.
While you consider checking the in-flight menu for one of those drinks that come
with a little umbrella, you are interrupted by the kid sitting next to you.

Their handheld game console won't turn on! They ask if you can take a look.

You narrow the problem down to a strange infinite loop in the boot code (your
puzzle input) of the device. You should be able to fix it, but first you need to
be able to run the code in isolation.

The boot code is represented as a text file with one instruction per line of
text. Each instruction consists of an operation (acc, jmp, or nop) and an
argument (a signed number like +4 or -20).

    - acc increases or decreases a single global value called the accumulator by
      the value given in the argument. For example, acc +7 would increase the
      accumulator by 7. The accumulator starts at 0. After an acc instruction,
      the instruction immediately below it is executed next.

    - jmp jumps to a new instruction relative to itself. The next instruction to
      execute is found using the argument as an offset from the jmp instruction;
      for example, jmp +2 would skip the next instruction, jmp +1 would continue
      to the instruction immediately below it, and jmp -20 would cause the
      instruction 20 lines above to be executed next.

    - nop stands for No OPeration - it does nothing. The instruction immediately
      below it is executed next.

For example, consider the following program:

------------------------------------------------------------
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
------------------------------------------------------------

These instructions are visited in this order:

------------------------------------------------------------
nop +0  | 1
acc +1  | 2, 8(!)
jmp +4  | 3
acc +3  | 6
jmp -3  | 7
acc -99 |
acc +1  | 4
jmp -4  | 5
acc +6  |
------------------------------------------------------------

First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1
(acc +1) and jmp +4 sets the next instruction to the other acc +1 near the
bottom. After it increases the accumulator from 1 to 2, jmp -4 executes, setting
the next instruction to the only acc +3. It sets the accumulator to 5, and jmp
-3 causes the program to continue back at the first acc +1.

This is an infinite loop: with this sequence of jumps, the program will run
forever. The moment the program tries to run any instruction a second time, you
know it will never terminate.

Immediately before the program would run an instruction a second time, the value
in the accumulator is 5.

Run your copy of the boot code. Immediately before any instruction is executed a
second time, what value is in the accumulator?
*)

(* The regexp syntax is that of https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html *)
module Str = Humane_re.Str

(* TODO: learn more about mutually recursive modules etc to model the CPU,
         program and instructions the best way *)

module CPU = struct
  type insn =
    | ACC of int
    | JMP of int
    | NOP of int

  type regs = {
    pc  :int;
    acc :int;
  }

  (* Initial register file state *)
  let init = {
    pc = 0;
    acc = 0;
  }

  (* Executes a single instruction and returns the modified register file. *)
  let exec regs = function
    | ACC v -> {pc = regs.pc + 1; acc = regs.acc + v}
    | JMP v -> {regs with pc = regs.pc + v}
    | NOP _ -> {regs with pc = regs.pc + 1}
end

module Program = struct
  type cell = {
    hit:  int; (* Execution count *)
    insn: CPU.insn;
  }

  let make insn_list =
    let len = List.length insn_list in
    let vec = Array.make len {hit = 0; insn = NOP(0)} in
    let rec fill_vec i = function
      | insn :: xs -> vec.(i) <- {hit = 0; insn}; fill_vec (i+1) xs
      | [] -> ()
    in
    fill_vec 0 insn_list; vec

  let clear_hit_counts (prog: cell array) =
    let len = Array.length prog in
    let rec aux i =
      if i < len then
        let t = prog.(i) in
        prog.(i) <- {t with hit = 0};
        aux (i+1)
      else ()
    in
    aux 0

  let fetch (prog: cell array) pc = prog.(pc)

  let store (prog: cell array) pc cell = prog.(pc) <- cell
end

let re_insn = Str.regexp "^\\(acc\\|jmp\\|nop\\) \\([+-][0-9]+\\)$"

let parse_input input =
  let make_insn op arg =
    let arg = int_of_string arg in
    match op with
    | "acc" -> CPU.ACC(arg)
    | "jmp" -> CPU.JMP(arg)
    | "nop" -> CPU.NOP(arg)
    | _ -> CPU.NOP(0)
  in
  let rec aux acc = function
    | x :: xs ->
      begin
        match Str.find_concat_groups re_insn x with
        | op :: arg :: [] -> aux ((make_insn op arg) :: acc) xs
        | _ -> aux acc xs (* discard *)
      end
    | [] -> acc
  in
  List.rev (aux [] input)

let exec_program prg =
  let prg_len = Array.length prg in
  let rec aux (regs :CPU.regs) prg =
    if regs.pc < 0 || regs.pc >= prg_len then
      regs
    else
      let cell = Program.fetch prg regs.pc in
      if cell.hit > 0 then
        regs (* cycle detected *)
      else begin
        Program.store prg regs.pc {cell with hit = cell.hit + 1};
        aux (CPU.exec regs cell.insn) prg
      end
  in
  aux CPU.init prg

let part1 (args :string list) :string =
  match args with
  | filename :: [] ->
    let program =
      File.lines filename |>
      parse_input |>
      Program.make
    in
    (exec_program program).acc |> string_of_int
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))

let part2 (args :string list) :string =
  match args with
  | filename :: [] ->
    let program =
      File.lines filename |>
      parse_input |>
      Program.make
    in
    let prg_len = Array.length program in
    let mutate_insn = function
      | CPU.JMP(v) -> CPU.NOP(v)
      | CPU.NOP(v) -> CPU.JMP(v)
      | _ as x -> x
    in
    (* When in doubt, use brute force *)
    let rec mutate_and_test pc =
      if pc >= prg_len then
        None
      else begin
        Program.clear_hit_counts program;
        let cell = Program.fetch program pc in
        Program.store program pc {cell with insn = (mutate_insn cell.insn)};
        let regs = exec_program program in
        Program.store program pc cell;
        if regs.pc == prg_len then
          Some regs
        else
          mutate_and_test (pc + 1)
      end
    in
    begin
      match mutate_and_test 0 with
      | Some(regs) -> string_of_int regs.acc
      | None -> "Bad luck!"
    end
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))
