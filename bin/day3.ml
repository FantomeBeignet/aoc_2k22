let line_to_backpack line =
  let size = (String.length line) / 2 in
  (String.sub line 0 size, String.sub line size size);;

let common_item (comp1, comp2) =
  let rec aux = function
    | h::_ when String.contains comp2 h -> Some h;
    | _::t -> aux t;
    | [] -> None
  in aux (List.init (String.length comp1) (String.get comp1));;

let priority = function
  | None -> 0
  | Some a -> 1 + String.index "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" a;;

let group_input lines =
  let rec aux buff = function
    | a::b::c::t -> aux ((a, b, c)::buff) t;
    | [] -> buff;
    | _ -> raise (Invalid_argument "invalid input length")
  in aux [] lines;;

let common_item3 (bp1, bp2, bp3) = 
  let rec aux = function
    | h::_ when String.contains bp2 h && String.contains bp3 h -> Some h;
    | _::t -> aux t;
    | [] -> None
  in aux (List.init (String.length bp1) (String.get bp1));;


let () =
  let parsed = "files/day3.txt" |> Aoc_2k22.read_lines in
  parsed |> List.map line_to_backpack |> List.map common_item |> List.map priority |> Aoc_2k22.sum_list |> print_int;
  print_endline "";
  parsed |> group_input |> List.map common_item3 |> List.map priority |> Aoc_2k22.sum_list |> print_int;;
