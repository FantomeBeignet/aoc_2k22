let is_marker str =
  let rec aux buff s =
    match String.length s with
    | 0 -> true
    | _ when String.contains buff (String.get s 0) -> false
    | _ -> aux (buff ^ (String.make 1 (String.get s 0))) (String.sub s 1 (String.length s - 1))
 in aux "" str;;

let find_marker marker_len str =
  let rec aux i s =
    match String.length s with
    | 0 -> failwith "What";
    | _ when is_marker (String.sub s 0 marker_len) -> i + marker_len;
    | _ -> aux (i + 1) (String.sub s 1 (String.length s - 1))
  in aux 0 str;;

let () =
  let signal = "files/day6.txt" |> Aoc_2k22.read_lines |> List.hd in
  signal |> find_marker 4 |> print_int;
  print_newline ();
  signal |> find_marker 14 |> print_int;;
