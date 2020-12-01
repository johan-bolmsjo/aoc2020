let line_stream_of_channel chan =
  Stream.from (fun _ -> try Some (input_line chan) with End_of_file -> None)

let lines (filename :string) :string list =
  let ichan = open_in filename in
  let line_stream = line_stream_of_channel ichan in
  let rec slurp (acc :string list) :string list =
    try
      slurp (Stream.next line_stream :: acc)
    with Stream.Failure -> acc
  in
  let lines = List.rev (slurp []) in
  close_in ichan; lines
