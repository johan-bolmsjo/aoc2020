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

let test_day5_part1 () =
  Alcotest.(check string) "equal" "991" (Day5.part1 ["../../../data/day5/input"])

let test_day5_part2 () =
  Alcotest.(check string) "equal" "534" (Day5.part2 ["../../../data/day5/input"])

let test_day6_part1 () =
  Alcotest.(check string) "equal" "6903" (Day6.part1 ["../../../data/day6/input"])

let test_day6_part2 () =
  Alcotest.(check string) "equal" "3493" (Day6.part2 ["../../../data/day6/input"])

let test_day7_part1 () =
  Alcotest.(check string) "equal" "272" (Day7.part1 ["../../../data/day7/input"])

let test_day7_part2 () =
  Alcotest.(check string) "equal" "172246" (Day7.part2 ["../../../data/day7/input"])

let test_day8_part1 () =
  Alcotest.(check string) "equal" "1810" (Day8.part1 ["../../../data/day8/input"])

let test_day8_part2 () =
  Alcotest.(check string) "equal" "969" (Day8.part2 ["../../../data/day8/input"])

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
      ( "Day5",
        [
          Alcotest.test_case "part1" `Quick test_day5_part1;
          Alcotest.test_case "part2" `Quick test_day5_part2;
        ] );
      ( "Day6",
        [
          Alcotest.test_case "part1" `Quick test_day6_part1;
          Alcotest.test_case "part2" `Quick test_day6_part2;
        ] );
      ( "Day7",
        [
          Alcotest.test_case "part1" `Quick test_day7_part1;
          Alcotest.test_case "part2" `Quick test_day7_part2;
        ] );
      ( "Day8",
        [
          Alcotest.test_case "part1" `Quick test_day8_part1;
          Alcotest.test_case "part2" `Quick test_day8_part2;
        ] );
      (*
      ( "DayX",
        [
          Alcotest.test_case "part1" `Quick test_dayX_part1;
          Alcotest.test_case "part2" `Quick test_dayX_part2;
        ] );
      *)
    ]
