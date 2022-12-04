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

let max3 list =
  let rec aux (m1, m2, m3) = function
    | [] -> (m1, m2, m3);
    | h::t when h > m1 -> aux (h, m1, m2) t;
    | h::t when h > m2 -> aux (m1, h, m2) t;
    | h::t when h > m3 -> aux (m1, m2, h) t;
    | _::t -> aux (m1, m2, m3) t
  in aux (-1, -1, -1) list;;

let sum3 = function
  | (x, y, z) -> x + y + z;;

let () =
  let grouped = "files/day1.txt" |> Aoc_2k22.read_lines |> group_calories in
  grouped |> max_calories |> print_int;
  print_newline ();
  grouped |> max3 |> sum3 |> print_int;;
