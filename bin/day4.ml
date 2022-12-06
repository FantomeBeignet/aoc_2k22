type section = { first : int; last : int }

let contains (sec1, sec2) =
  match sec1 with
  | { first = f; last = l } when f <= sec2.first && l >= sec2.last -> 1
  | { first = f; last = l } when f >= sec2.first && l <= sec2.last -> 1
  | _ -> 0

let parse_section = function
  | [ a; b ] -> { first = int_of_string a; last = int_of_string b }
  | _ -> failwith "Invalid section format"

let line_to_sections line =
  let split_line = String.split_on_char ',' line in
  match split_line with
  | [ a; b ] ->
      ( parse_section (String.split_on_char '-' a),
        parse_section (String.split_on_char '-' b) )
  | _ -> failwith "Invalid line format"

let overlaps (sec1, sec2) =
  match sec1 with
  | { first = f; last = l } when f <= sec2.first && l >= sec2.first -> 1
  | { first = f; last = l } when f <= sec2.last && l >= sec2.last -> 1
  | { first = f; last = l } when f <= sec2.first && l >= sec2.last -> 1
  | { first = f; last = l } when f >= sec2.first && l <= sec2.last -> 1
  | _ -> 0

let () =
  let parsed =
    "files/day4.txt" |> Aoc_2k22.read_lines |> List.map line_to_sections
  in
  parsed |> List.map contains |> Aoc_2k22.sum_list |> print_int;
  print_newline ();
  parsed |> List.map overlaps |> Aoc_2k22.sum_list |> print_int
