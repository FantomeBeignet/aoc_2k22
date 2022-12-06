type shape = Rock | Paper | Scissors
type outcome = Win | Draw | Loss

let letter_to_shape = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "Unknown shape"

let shape_to_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let parse_line1 line =
  (String.get line 0 |> letter_to_shape, String.get line 2 |> letter_to_shape)

let get_outcome = function
  | Rock, Paper -> Win
  | Rock, Scissors -> Loss
  | Paper, Rock -> Loss
  | Paper, Scissors -> Win
  | Scissors, Rock -> Win
  | Scissors, Paper -> Loss
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw

let outcome_to_score = function Win -> 6 | Draw -> 3 | Loss -> 0

let score_match duel =
  shape_to_score (snd duel) + (get_outcome duel |> outcome_to_score)

let letter_to_outcome = function
  | 'X' -> Loss
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "Unknown strat"

let parse_line2 line =
  (String.get line 0 |> letter_to_shape, String.get line 2 |> letter_to_outcome)

let outcome_to_strategy = function
  | Rock, Win | Scissors, Loss | Paper, Draw -> Paper
  | Paper, Win | Rock, Loss | Scissors, Draw -> Scissors
  | Scissors, Win | Rock, Draw | Paper, Loss -> Rock

let strat1_to_strat2 strat = (fst strat, outcome_to_strategy strat)

let () =
  let parsed = "files/day2.txt" |> Aoc_2k22.read_lines in
  parsed |> List.map parse_line1 |> List.map score_match
  |> List.fold_left (fun a x -> a + x) 0
  |> print_int;
  print_newline ();
  parsed |> List.map parse_line2 |> List.map strat1_to_strat2
  |> List.map score_match
  |> List.fold_left (fun a x -> a + x) 0
  |> print_int
