let backtrace () = Printexc.print_backtrace stderr

let fatalln msg =
  print_endline msg;
  exit 1
