(*
--- Day 7: Handy Haversacks ---

You land at the regional airport in time for your next flight. In fact, it looks
like you'll even have time to grab some food: all flights are currently delayed
due to issues in luggage processing.

Due to recent aviation regulations, many rules (your puzzle input) are being
enforced about bags and their contents; bags must be color-coded and must
contain specific quantities of other color-coded bags. Apparently, nobody
responsible for these regulations considered how long they would take to
enforce!

For example, consider the following rules:

------------------------------------------------------------
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
------------------------------------------------------------

These rules specify the required contents for 9 bag types. In this example,
every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded
blue and 6 dotted black), and so on.

You have a shiny gold bag. If you wanted to carry it in at least one other bag,
how many different bag colors would be valid for the outermost bag? (In other
words: how many colors can, eventually, contain at least one shiny gold bag?)

In the above rules, the following options would be available to you:

    - A bright white bag, which can hold your shiny gold bag directly.
    - A muted yellow bag, which can hold your shiny gold bag directly, plus some
      other bags.
    - A dark orange bag, which can hold bright white and muted yellow bags,
      either of which could then hold your shiny gold bag.
    - A light red bag, which can hold bright white and muted yellow bags, either
      of which could then hold your shiny gold bag.

So, in this example, the number of bag colors that can eventually contain at
least one shiny gold bag is 4.

How many bag colors can eventually contain at least one shiny gold bag? (The
list of rules is quite long; make sure you get all of it.)

--- Part Two ---

It's getting pretty expensive to fly these days - not because of ticket prices,
but because of the ridiculous number of bags you need to buy!

Consider again your shiny gold bag and the rules from the above example:

    - faded blue bags contain 0 other bags.
    - dotted black bags contain 0 other bags.
    - vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted
      black bags.
    - dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black
      bags.

So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within
it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2
+ 2*11 = 32 bags!

Of course, the actual rules have a small chance of going several levels deeper
than this example; be sure to count all of the bags, even if the nesting becomes
topologically impractical!

Here's another example:

------------------------------------------------------------
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
------------------------------------------------------------

In this example, a single shiny gold bag must contain 126 other bags.

How many individual bags are required inside your single shiny gold bag?
*)

module Lexer = struct
  include Lexer

  type token =
    | Identifier of datum
    | Number of datum
    | Comma of datum
    | Dot of datum
    | Space of datum
    | Newline of datum

  let datum_of_token = function
    | Identifier d | Number d | Comma d | Dot d | Space d | Newline d -> d

  let string_of_token token =
    let kind = match token with
      | Identifier _ -> "Identifier"
      | Number _     -> "Number"
      | Comma _      -> "Comma"
      | Dot _        -> "Dot"
      | Space _      -> "Space"
      | Newline _    -> "Newline"
    in
    let datum = datum_of_token token in
    Printf.sprintf "%s: \"%s\" %s" kind (String.escaped datum.value) (Location.string_of datum.location)

  let lex_identifier ctx = lex_until_or_eof ctx (fun c -> c >= 'a' && c <= 'z')
  let lex_number     ctx = lex_until_or_eof ctx (fun c -> c >= '0' && c <= '9')
  let lex_space      ctx = lex_until_or_eof ctx (fun c -> c = ' ' || c = '\t')
  let lex_newline    ctx = lex_until_or_eof ctx (fun c -> c = '\n' || c = '\r')

  let lex ctx =
    Context.reset ctx;
    match Context.peek_char ctx with
    | Some c ->
      begin
        Some(
          match c with
          | 'a' .. 'z'  -> Identifier(lex_identifier ctx c)
          | '0' .. '9'  -> Number(lex_number ctx c)
          | ','         -> Comma(lex_single_char ctx c)
          | '.'         -> Dot(lex_single_char ctx c)
          | ' ' | '\t'  -> Space(lex_space ctx c)
          | '\n' | '\r' -> Newline(lex_newline ctx c)
          | _ -> raise (Error.Exn(With_loc(Bad_token, Context.current_location ctx)))
        )
      end
    | None -> None (* EOF *)
end

module Parser = struct
  module String_map = Map.Make(String)
  module String_set = Set.Make(String)

  type state = {
    count       : int;
    words       : string list;
    outer_color : string;

    (* Maps color to list of colors its contained in *)
    reverse_colormap : string list String_map.t;

    (* Maps color to counts and colors it contains *)
    colormap : (int * string) list String_map.t;
  }

  let init_state = {
    count = 0;
    words = [];
    outer_color = "";
    reverse_colormap = String_map.empty;
    colormap = String_map.empty;
  }

  let state_color state = BatString.join " " (List.rev state.words)

  let parse (token : Lexer.token) state : state =
    match token with
    | Identifier d ->
      begin
        match d.value with
        | "bag" | "bags" -> state (* superfluous word *)
        | "contain" | "contains" ->
          let outer_color = state_color state in
          {state with count = 0; words = []; outer_color}
        | _ as v -> {state with words = v :: state.words}
      end
    | Number d -> {state with count = int_of_string d.value}
    | Comma _ | Dot _ ->
      let color = state_color state in
      if color = "no other" then
        {state with words = []}
      else
        let reverse_colormap = String_map.update color (fun d ->
            match d with
            | Some d -> Some(state.outer_color :: d)
            | None -> Some [state.outer_color]
          ) state.reverse_colormap
        in
        let colormap = String_map.update state.outer_color (fun d ->
            let elt = (state.count, color) in
            match d with
            | Some d -> Some(elt :: d)
            | None -> Some [elt]
          ) state.colormap
        in
        {state with words = []; reverse_colormap; colormap}
    | Space _ -> state
    | Newline _ -> state

  let count_bags_containing_color color state =
    let rec scan_map visited color =
      match String_map.find_opt color state.reverse_colormap with
      | Some colors -> scan_list visited colors
      | None -> visited
    and scan_list visited = function
      | color :: rest ->
        if String_set.mem color visited then
          scan_list visited rest
        else
          let visited = String_set.add color visited in
          (* not tail recursive *)
          scan_list (scan_map visited color) rest
      | [] -> visited
    in
    let visited = scan_map String_set.empty color in
    String_set.cardinal visited

  (* TODO: protect against self referencing bags *)
  let count_bags_contained_in_color color state =
    let rec scan_map color =
      match String_map.find_opt color state.colormap with
      | Some colors -> scan_list colors
      | None -> 0
    and scan_list = function
      | (n, color) :: rest ->
        n + (n * (scan_map color)) + (scan_list rest)
      | [] -> 0
    in
    scan_map color

end

let backtrace () = Printexc.print_backtrace stderr

let fatal msg =
  print_endline msg;
  exit 1

let lexer_debug = false

(* TODO: file descriptor leak *)
let parse_file filename  =
  let ichan = open_in filename in
  let lex_ctx = Lexer.Context.of_char_stream (Stream.of_channel ichan) in
  let rec lex pstate =
    match Lexer.lex lex_ctx with
    | Some token ->
      (if lexer_debug then print_endline (Lexer.string_of_token token));
      lex (Parser.parse token pstate)
    | None -> pstate
  in
  try
    (lex Parser.init_state)
  with
  | Lexer.Error.Exn(err) -> backtrace (); fatal (Lexer.Error.string_of err)

let part1 (args :string list) :string =
  match args with
  | filename :: [] ->
    parse_file filename |>
    Parser.count_bags_containing_color "shiny gold" |>
    string_of_int
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))

let part2 (args :string list) :string =
  match args with
  | filename :: [] ->
    parse_file filename |>
    Parser.count_bags_contained_in_color "shiny gold" |>
    string_of_int
  | _ -> raise (Error.Bad_arg("Puzzle requires data file argument."))
