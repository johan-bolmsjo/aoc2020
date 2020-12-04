open Aoc2020

let test_day1_part1 () =
  Alcotest.(check string) "equal" "1007331" (Day1.part1 ["../../../data/day1/input"])

let test_day1_part2 () =
  Alcotest.(check string) "equal" "48914340" (Day1.part2 ["../../../data/day1/input"])

let test_day2_part1 () =
  Alcotest.(check string) "equal" "398" (Day2.part1 ["../../../data/day2/input"])

let test_day2_part2 () =
  Alcotest.(check string) "equal" "562" (Day2.part2 ["../../../data/day2/input"])

let test_day3_part1 () =
  Alcotest.(check string) "equal" "145" (Day3.part1 ["../../../data/day3/input"])

let test_day3_part2 () =
  Alcotest.(check string) "equal" "3424528800" (Day3.part2 ["../../../data/day3/input"])

let test_day4_part1 () =
  Alcotest.(check string) "equal" "226" (Day4.part1 ["../../../data/day4/input"])

let test_day4_part2 () =
  Alcotest.(check string) "equal" "160" (Day4.part2 ["../../../data/day4/input"])

(*
let test_dayX_part1 () =
  Alcotest.(check string) "equal" "" (DayX.part1 ["../../../data/dayX/input"])

let test_dayX_part2 () =
  Alcotest.(check string) "equal" "" (DayX.part2 ["../../../data/dayX/input"])
*)

let () =
  Alcotest.run "Puzzle result tests"
    [
      ( "Day1",
        [
          Alcotest.test_case "part1" `Quick test_day1_part1;
          Alcotest.test_case "part2" `Quick test_day1_part2;
        ] );
      ( "Day2",
        [
          Alcotest.test_case "part1" `Quick test_day2_part1;
          Alcotest.test_case "part2" `Quick test_day2_part2;
        ] );
      ( "Day3",
        [
          Alcotest.test_case "part1" `Quick test_day3_part1;
          Alcotest.test_case "part2" `Quick test_day3_part2;
        ] );
      ( "Day4",
        [
          Alcotest.test_case "part1" `Quick test_day4_part1;
          Alcotest.test_case "part2" `Quick test_day4_part2;
        ] );
      (*
      ( "DayX",
        [
          Alcotest.test_case "part1" `Quick test_dayX_part1;
          Alcotest.test_case "part2" `Quick test_dayX_part2;
        ] );
      *)
    ]
