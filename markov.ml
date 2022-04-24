let () = Random.self_init ()

let markov_table : (string * string, string list) Hashtbl.t = Hashtbl.create 4096

let update table key ~f =
  match f (Hashtbl.find_opt table key) with
  | None -> Hashtbl.remove table key
  | Some data -> Hashtbl.add table key data

let keys table = Hashtbl.fold (fun k _ acc -> k :: acc) table []

let read_file filename =
  let ch = open_in filename in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let prepare_corpus file_path =
  file_path
  |> read_file
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line -> String.length line > 0)
  |> List.map (fun line ->
               line
               |> String.split_on_char ' '
               |> List.map String.trim
               |> List.filter (fun line -> String.length line > 0))
  |> List.flatten

(* TODO: save the hashtable? *)
let rec build_table words table =
  match words with
  | []
  | _ :: []
  | _ :: _ :: [] -> ()
  | w1 :: w2 :: w3 :: rest ->
    update table (w1, w2) (function
                           | Some x -> Some (x @ [w3])
                           | None -> Some [w3]);
    build_table (w2 :: w3 :: rest) table

let get_random_element list =
  let i = Random.int (List.length list) in
  List.nth_opt list i

let get_random_pair table =
  get_random_element (keys table)

let markov ?(length=15) table =
  let first =
    match get_random_pair table with
    | Some pair -> pair
    | None -> ("", "")
  in
  let rec aux key first length chain =
    match length with
    | 0 -> chain
    | _ ->
      let (first, second) = key in
      let next =
        match Hashtbl.find_opt table key with
        | Some words -> get_random_element words
        | None -> None
      in
      match next with
      | Some word -> aux (second, word) table (length - 1) ((chain ^ word) ^ " ")
      | None -> chain
   in aux first table length  ""

let () =
  match Array.to_list Sys.argv with
  | _ :: path :: _ ->
    let words = prepare_corpus path in
    build_table words markov_table;

    let chain = markov markov_table in
    print_endline chain
  | _ -> print_endline "USAGE: ./markov corpus.txt";
