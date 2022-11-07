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


module Config = struct
  type t = {
    corpus_path : string;
    output_path : string option;
    model_path : string option;
    length : int;
  }

  let make () =
    let usage = "./markov <path/to/corpus.txt> -l <lenght> -m <path/to/model.bin> -o <path/to/model.bin>" in
    let corpus_path = ref "" in
    let model_path = ref "" in
    let output_path = ref "" in
    let length = ref 50 in

    let anon_fun filename =
      corpus_path := filename
    in

    let speclist =
      [("-l", Arg.Set_int length, "Length of the generated text");
       ("-o", Arg.Set_string output_path, "Output path for the model");
       ("-m", Arg.Set_string model_path, "Load a model from a file")]
    in

    Arg.parse speclist anon_fun usage;

    {
      corpus_path = !corpus_path;
      model_path =  if !model_path = "" then None else Some !model_path;
      output_path = if !output_path = "" then None else Some !output_path;
      length = !length;
    }
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
  let config = Config.make () in

  let tabl =
    match config.model_path with
    | Some path -> MarkovTable.load_from_file path
    | None ->
      let tabl = MarkovTable.make () in
      let corpus = prepare_corpus config.corpus_path in
      MarkovTable.init tabl corpus;
      tabl
  in

  match config.output_path with
  | Some path -> MarkovTable.save_to_file tabl path
  | None -> ();

  let chain = markov ~length:config.length tabl in
  print_endline chain
