(* Log exception backtrace to stderr *)
val backtrace: unit -> unit

(* Log message and exit with status code 1 *)
val fatalln: string -> 'a
