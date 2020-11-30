open Aoc2020

let test_main () =
  Alcotest.(check string) "same string" "todo" (Day1.main [])

let () =
  Alcotest.run "Day1 tests"
    [
      ( "Day1",
        [ Alcotest.test_case "main" `Quick test_main ] );
    ]
