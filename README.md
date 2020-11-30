# Advent of Code (AoC) 2020

This year's advent of code will be done in OCaml.
I would rate myself as a novice OCaml coder.
Let's see how far the reindeer flies.

Godspeed.

## Build

A working OCaml installation is required, see https://ocaml.org/.

Make sure the following packages are installed on your system (using opam).

* alcotest
* dune

Build and run puzzles:

    dune build
    dune exec --debug-backtraces -- aoc2020 N ARGS
	
Where N is the calendar day and ARGS any required puzzle arguments.
Puzzles output a usage message if required arguments are missing.

Run tests:

    dune runtest
