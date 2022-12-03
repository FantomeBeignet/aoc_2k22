let group_calories elves =
  let rec aux buff = function
    | h::t when h = "" -> aux (0::buff) t;
    | h::t -> aux ((List.hd buff + int_of_string h)::(List.tl buff)) t;
    | [] -> buff
  in aux (0::[]) elves;;

let max_calories calories =
  let rec aux buff = function
    | h::t when h > buff -> aux h t;
    | _::t -> aux buff t;
    | [] -> buff
  in aux (-1) calories;;

let () =
  "files/day1.txt" |> Aoc_2k22.read_lines |> group_calories |> max_calories |> print_int;;
