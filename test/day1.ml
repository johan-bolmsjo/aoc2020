open Aoc2020

let test_part1 () =
  Alcotest.(check string) "equal" "1007331" (Day1.part1 ["../../../data/day1/input"])

let test_part2 () =
  Alcotest.(check string) "equal" "48914340" (Day1.part2 ["../../../data/day1/input"])

let () =
  Alcotest.run "Day1 tests"
    [
      ( "Day1",
        [
          Alcotest.test_case "part1" `Quick test_part1;
          Alcotest.test_case "part2" `Quick test_part2;
        ] );
    ]
