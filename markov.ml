let () = Random.self_init ()

exception Unreachable

let read_file filename =
  let ch = open_in filename in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let get_random_element list =
  let i = Random.int (List.length list) in
  List.nth list i

module MarkovTable : sig
  type key = (string * string)
  type value = string list

  type t

  val make : ?size:int -> unit -> t

  val ( .%{} ) : t -> key -> value option
  val ( .%{} <- ) : t -> key -> value -> unit

  val keys : t -> key list

  val update : t -> key -> f:(value option -> value option) -> unit

  val init : t -> string list -> unit

  val random_key : t -> key

  val save_to_file : t -> string -> unit

  val load_from_file : string -> t
end = struct
  include Hashtbl

  type key = (string * string)
  type value = string list

  type t = (key, value) Hashtbl.t

  let make ?(size=4096) () = create size

  let ( .%{} ) tabl index = find_opt tabl index
  let ( .%{} <- ) tabl index value = add tabl index value

  let keys tabl = fold (fun k _ acc -> k :: acc) tabl []

  let update tabl key ~f =
    match f (tabl.%{key}) with
    | None -> remove tabl key
    | Some value -> tabl.%{key} <- value

  let rec init tabl words =
    match words with
    | []
    | _ :: []
    | _ :: _ :: [] -> ()
    | w1 :: w2 :: w3 :: rest ->
      update tabl ((w1, w2)) (function
                              | Some value -> Some (value @ [w3])
                              | None -> Some [w3]);
      init tabl (w2 :: w3 :: rest)

  let random_key tabl = get_random_element (keys tabl)

  let save_to_file tabl path =
    let ch = open_out_bin path in
    Marshal.to_channel ch tabl [];
    close_out ch

  let load_from_file path =
    let ch = open_in_bin path in
    let tabl = Marshal.from_channel ch in
    close_in ch;
    tabl
end

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

let markov ~length tabl =
  let first = MarkovTable.random_key tabl in
  let rec aux key tabl length chain =
    match length with
    | 0 -> chain
    | _ ->
      let (_, w2) = key in
      let next =
        match MarkovTable.(tabl.%{key}) with
        | Some words -> get_random_element words
        | None -> raise Unreachable
      in

      aux (w2, next) tabl (length - 1) ((chain ^ next) ^ " ")
   in aux first tabl length  ""

let () =
  match Array.to_list Sys.argv with
  | _ :: path :: length :: _ ->
    let table = MarkovTable.make () in
    let words = prepare_corpus path in

    MarkovTable.init table words ;

    let chain = markov ~length:(int_of_string length) table in
    print_endline chain
  | _ -> print_endline "USAGE: ./markov corpus.txt <nº of words>";
