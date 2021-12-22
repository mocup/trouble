open Array
open ANSITerminal
open State
open Specs

(*[print_space row col color] prints the board space located at (row,
  col) and containing either player [color] or nothing.*)
let print_space row col color =
  let print_bracket str =
    if is_top_left row col then
      ANSITerminal.(print_string [ yellow ] str)
    else if is_top_right row col then
      ANSITerminal.(print_string [ blue ] str)
    else if is_bottom_left row col then
      ANSITerminal.(print_string [ green ] str)
    else if is_bottom_right row col then
      ANSITerminal.(print_string [ red ] str)
    else ANSITerminal.(print_string [ white ] str)
  in
  let num_to_letter = function
    | 0 -> "A"
    | 1 -> "B"
    | 2 -> "C"
    | 3 -> "D"
    | _ -> failwith "impossible"
  in
  let print_contents = function
    | None -> ANSITerminal.(print_string [ white ] " ")
    | Some (Red, i) ->
        ANSITerminal.(print_string [ red ] (num_to_letter i))
    | Some (Green, i) ->
        ANSITerminal.(print_string [ green ] (num_to_letter i))
    | Some (Blue, i) ->
        ANSITerminal.(print_string [ blue ] (num_to_letter i))
    | Some (Yellow, i) ->
        ANSITerminal.(print_string [ yellow ] (num_to_letter i))
  in
  print_bracket " [";
  print_contents color;
  print_bracket "] "

let rec print_board board die_val =
  for row = start_zone_index to end_start_zone_index do
    print_newline ();
    for col = start_zone_index to end_start_zone_index do
      match board.(row).(col) with
      | Ignore -> ANSITerminal.(print_string [ white ] "     ")
      | Space { contents } -> print_space row col contents
      | Die ->
          ANSITerminal.(
            print_string [ white ] (" [" ^ string_of_int die_val ^ "] "))
    done;
    print_newline ()
  done;
  ()
