let read_lines file =
  let ic = In_channel.open_text file in
  In_channel.input_all ic |> Str.split (Str.regexp "\n")

let sum_list =
  List.fold_left (fun a x -> a + x) 0;;
