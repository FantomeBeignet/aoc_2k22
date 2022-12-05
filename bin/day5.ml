type move = {qty : int; from_stack : int; to_stack : int};;

let split_line_in_slots line =
  let rec aux buff line =
    match String.length line with
    | a when a > 3 -> aux ((String.get line 1)::buff) (String.sub line 4 (String.length line - 4));
    | a when a = 3 -> ((String.get line 1)::buff);
    | _ -> failwith "What"
  in List.rev (aux [] line);;

let line_to_stacks stack_arr line =
  let add_to_stack arr i elt =
    Stack.push elt (Array.get arr i)
  in 
  let rec aux index = function
    | h::t when h = ' ' -> aux (index + 1) t;
    | h::t -> add_to_stack stack_arr index h; aux (index + 1) t;
    | [] -> ()
  in aux 0 line;;

let parse_move move =
  let move_pattern = Str.regexp {|move\ \([0-9]+\)\ from\ \([0-9]+\)\ to\ \([0-9]+\)|} in
  let _ = Str.string_match move_pattern move 0 in
  {qty = int_of_string (Str.matched_group 1 move); from_stack = int_of_string (Str.matched_group 2 move) - 1; to_stack = int_of_string (Str.matched_group 3 move) - 1};;
  
let apply_move stacks move =
  for _ = 1 to move.qty do
    let popped = Stack.pop (Array.get stacks (move.from_stack)) in
    Stack.push popped (Array.get stacks (move.to_stack))
  done

let () =
  let stacks = (Array.init 9 (fun _ -> (Stack.create ()))) in
  "files/day5_init.txt" |> Aoc_2k22.read_lines |> List.rev |> List.map split_line_in_slots |> List.iter (line_to_stacks stacks);
  "files/day5_steps.txt" |> Aoc_2k22.read_lines |> List.map parse_move |> List.iter (apply_move stacks);
  Array.iter (fun s -> print_char (Stack.top s)) stacks;;
