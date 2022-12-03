type shape = Rock | Paper | Scissors;;

type outcome = Win | Draw | Loss;;

exception Wrong_shape of string;;

let letter_to_shape = function
  | 'A' | 'X' ->  Rock;
  | 'B' | 'Y' ->  Paper;
  | 'C' | 'Z' -> Scissors;
  | _ -> raise (Wrong_shape "Unknown shape");;

let shape_to_score = function
  | Rock -> 1;
  | Paper -> 2;
  | Scissors -> 3;;

let parse_line line =
  (String.get line 0 |> letter_to_shape, String.get line 2 |> letter_to_shape);; 

let get_outcome = function
  | Rock, Paper ->  Win;
  | Rock, Scissors -> Loss;
  | Paper, Rock -> Loss;
  | Paper, Scissors -> Win;
  | Scissors, Rock -> Win;
  | Scissors, Paper -> Loss;
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw;;

let outcome_to_score = function
  | Win -> 6;
  | Draw -> 3;
  | Loss -> 0;;

let score_match duel =
  shape_to_score (snd duel) + (get_outcome duel |> outcome_to_score);;

let () =
  "files/day2.txt" |> Aoc_2k22.read_lines |> List.map parse_line |> List.map score_match |> List.fold_left (fun a x -> a + x) 0 |> print_int;;
