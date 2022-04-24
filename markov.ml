let read_file filename =
  let ch = open_in filename in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let prepare_corpus file =
  file
  |> read_file
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line -> String.length line > 0)
  |> List.map @@ fun line ->
                 line
                 |> String.split_on_char ' '
                 |> List.map String.trim
                 |> List.filter (fun line -> String.length line > 0)

let usage = "USAGE: ./markov corpus.txt"
let () =
    match Array.to_list Sys.argv with
    | _ :: path :: _ -> print_endline "TODO: Markov chain is not implemented";
    | _ -> print_endline usage;
