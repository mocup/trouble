open State
open Array
open ANSITerminal
open Specs

let rec welcome () =
  ANSITerminal.(print_string [ white ] "\nWelcome to Trouble.\n");
  ANSITerminal.(
    print_string [ white ]
      "Bump others back in this race-ahead game...\n\
       but don't get caught or you're in Trouble!");
  ANSITerminal.(print_string [ green ] "\nPress S to start: \n");
  let input = read_line () in
  if input = "S" || input = "s" then ()
  else (
    ANSITerminal.(
      print_string [ red ] "\nInvalid command. Try again:\n");
    welcome ())

(**[is_top_left_start_zone row col] returns true if (row, col) is a top
   left start zone space, and false otherwise.*)
let is_top_left_start_zone row col =
  (row = start_zone_index && col = start_zone_index + 3)
  || (row = start_zone_index + 1 && col = start_zone_index + 2)
  || (row = start_zone_index + 2 && col = start_zone_index + 1)
  || (row = start_zone_index + 3 && col = start_zone_index)

(**[is_top_right_start_zone row col] returns true if (row, col) is a top
   right start zone space, and false otherwise.*)
let is_top_right_start_zone row col =
  (row = start_zone_index && col = end_start_zone_index - 3)
  || (row = start_zone_index + 1 && col = end_start_zone_index - 2)
  || (row = start_zone_index + 2 && col = end_start_zone_index - 1)
  || (row = start_zone_index + 3 && col = end_start_zone_index)

(**[is_bottom_left_start_zone row col] returns true if (row, col) is a
   bottom left start zone space, and false otherwise.*)
let is_bottom_left_start_zone row col =
  (row = end_start_zone_index - 3 && col = start_zone_index)
  || (row = end_start_zone_index - 2 && col = start_zone_index + 1)
  || (row = end_start_zone_index - 1 && col = start_zone_index + 2)
  || (row = end_start_zone_index && col = start_zone_index + 3)

(**[is_bottom_right_start_zone row col] returns true if (row, col) is a
   bottom right start zone space, and false other wise.*)
let is_bottom_right_start_zone row col =
  (row = end_start_zone_index && col = end_start_zone_index - 3)
  || (row = end_start_zone_index - 1 && col = end_start_zone_index - 2)
  || (row = end_start_zone_index - 2 && col = end_start_zone_index - 1)
  || (row = end_start_zone_index - 3 && col = end_start_zone_index)

(**[make_start_zone board row col next_row next_col] makes a start zone
   on [board] with coordinates (row, col) and next space (next_row,
   next_col).*)
let make_start_zone board row col next_row next_col =
  board.(row).(col) <-
    Space
      {
        contents = None;
        next = Some (next_row, next_col);
        end_next = None;
      }

(**[make_start_zones board] initializes the start zones on [board] with
   pointers to the board entrance points.*)
let make_start_zones board =
  for row = start_zone_index to end_start_zone_index do
    for col = start_zone_index to end_start_zone_index do
      if is_top_left_start_zone row col then
        make_start_zone board row col start_space_index
          (start_space_index + 1);
      if is_top_right_start_zone row col then
        make_start_zone board row col (start_space_index + 1)
          end_space_index;
      if is_bottom_left_start_zone row col then
        make_start_zone board row col (end_space_index - 1)
          start_space_index;
      if is_bottom_right_start_zone row col then
        make_start_zone board row col end_space_index
          (end_space_index - 1)
      else ()
    done
  done;
  ()

(**[make_top_row board] initializes the spaces on the top row of the
   board with pointers to spaces to the right.*)
let make_top_row board =
  for col = start_space_index + 1 to end_space_index - 1 do
    if col = end_space_index - 1 then
      board.(start_space_index).(col) <-
        Space
          {
            contents = None;
            next = Some (start_space_index + 1, col + 1);
            end_next = Some (start_space_index + 1, col);
          }
    else
      board.(start_space_index).(col) <-
        Space
          {
            contents = None;
            next = Some (start_space_index, col + 1);
            end_next = None;
          }
  done;
  ()

(**[make_right_col board] initializes the spaces on the right column of
   the board with pointers to spaces below.*)
let make_right_col board =
  for row = start_space_index + 1 to end_space_index - 1 do
    if row = end_space_index - 1 then
      board.(row).(end_space_index) <-
        Space
          {
            contents = None;
            next = Some (row + 1, end_space_index - 1);
            end_next = Some (row, end_space_index - 1);
          }
    else
      board.(row).(end_space_index) <-
        Space
          {
            contents = None;
            next = Some (row + 1, end_space_index);
            end_next = None;
          }
  done;
  ()

(**[make_bottom_row board] initializes the spaces on the bottom row of
   the board with pointers to spaces to the left.*)
let make_bottom_row board =
  for col = end_space_index - 1 downto start_space_index + 1 do
    if col = start_space_index + 1 then
      board.(end_space_index).(col) <-
        Space
          {
            contents = None;
            next = Some (end_space_index - 1, col - 1);
            end_next = Some (end_space_index - 1, col);
          }
    else
      board.(end_space_index).(col) <-
        Space
          {
            contents = None;
            next = Some (end_space_index, col - 1);
            end_next = None;
          }
  done;
  ()

(**[make_left_col board] initializes the spaces on the left column of
   the board with pointers to spaces above.*)
let make_left_col board =
  for row = end_space_index - 1 downto start_space_index + 1 do
    if row = start_space_index + 1 then
      board.(row).(start_space_index) <-
        Space
          {
            contents = None;
            next = Some (row - 1, start_space_index + 1);
            end_next = Some (row, start_space_index + 1);
          }
    else
      board.(row).(start_space_index) <-
        Space
          {
            contents = None;
            next = Some (row - 1, start_space_index);
            end_next = None;
          }
  done;
  ()

let is_end_of_end_zone row col =
  (row = middle - 1 && col = middle - 1)
  || (row = middle - 1 && col = middle + 1)
  || (row = middle + 1 && col = middle - 1)
  || (row = middle + 1 && col = middle + 1)

(**[make_end_zone board row col next_row next_col] initializes the
   spaces in the end zones with pointers toward spaces in the center of
   the board.*)
let make_end_zone board row col next_row next_col =
  if is_end_of_end_zone row col then
    board.(row).(col) <-
      Space { contents = None; next = None; end_next = None }
  else
    board.(row).(col) <-
      Space
        {
          contents = None;
          next = Some (next_row, next_col);
          end_next = None;
        }

(**[make_end_zones board] initializes all four end zones on [board].*)
let make_end_zones board =
  for row = start_space_index + 1 to end_space_index - 1 do
    for col = start_space_index + 1 to end_space_index - 1 do
      if is_top_left_end_zone row col then
        make_end_zone board row col (row + 1) (col + 1);
      if is_top_right_end_zone row col then
        make_end_zone board row col (row + 1) (col - 1);
      if is_bottom_left_end_zone row col then
        make_end_zone board row col (row - 1) (col + 1);
      if is_bottom_right_end_zone row col then
        make_end_zone board row col (row - 1) (col - 1)
      else ()
    done
  done;
  ()

let init_board () =
  let board = make_matrix board_length board_length Ignore in
  make_start_zones board;
  make_top_row board;
  make_right_col board;
  make_bottom_row board;
  make_left_col board;
  make_end_zones board;
  board.(middle).(middle) <- Die;
  board

let make_piece (loc_x, loc_y) (start_x, start_y) =
  { loc = (loc_x, loc_y); start = (start_x, start_y) }

(**[init_players ()] initializes a number of players on the board
   determined by the user.*)
let rec init_players () =
  let yellow =
    {
      color = Yellow;
      pieces =
        [|
          make_piece
            (start_zone_index + 3, start_zone_index)
            (start_zone_index + 3, start_zone_index);
          make_piece
            (start_zone_index + 2, start_zone_index + 1)
            (start_zone_index + 2, start_zone_index + 1);
          make_piece
            (start_zone_index + 1, start_zone_index + 2)
            (start_zone_index + 1, start_zone_index + 2);
          make_piece
            (start_zone_index, start_zone_index + 3)
            (start_zone_index, start_zone_index + 3);
        |];
    }
  in
  let blue =
    {
      color = Blue;
      pieces =
        [|
          make_piece
            (start_zone_index, end_start_zone_index - 3)
            (start_zone_index, end_start_zone_index - 3);
          make_piece
            (start_zone_index + 1, end_start_zone_index - 2)
            (start_zone_index + 1, end_start_zone_index - 2);
          make_piece
            (start_zone_index + 2, end_start_zone_index - 1)
            (start_zone_index + 2, end_start_zone_index - 1);
          make_piece
            (start_zone_index + 3, end_start_zone_index)
            (start_zone_index + 3, end_start_zone_index);
        |];
    }
  in
  let red =
    {
      color = Red;
      pieces =
        [|
          make_piece
            (end_start_zone_index, end_start_zone_index - 3)
            (end_start_zone_index, end_start_zone_index - 3);
          make_piece
            (end_start_zone_index - 1, end_start_zone_index - 2)
            (end_start_zone_index - 1, end_start_zone_index - 2);
          make_piece
            (end_start_zone_index - 2, end_start_zone_index - 1)
            (end_start_zone_index - 2, end_start_zone_index - 1);
          make_piece
            (end_start_zone_index - 3, end_start_zone_index)
            (end_start_zone_index - 3, end_start_zone_index);
        |];
    }
  in
  let green =
    {
      color = Green;
      pieces =
        [|
          make_piece
            (end_start_zone_index - 3, start_zone_index)
            (end_start_zone_index - 3, start_zone_index);
          make_piece
            (end_start_zone_index - 2, start_zone_index + 1)
            (end_start_zone_index - 2, start_zone_index + 1);
          make_piece
            (end_start_zone_index - 1, start_zone_index + 2)
            (end_start_zone_index - 1, start_zone_index + 2);
          make_piece
            (end_start_zone_index, start_zone_index + 3)
            (end_start_zone_index, start_zone_index + 3);
        |];
    }
  in
  let _ =
    ANSITerminal.(print_string [ white ] "\nHow many players?\n");
    ANSITerminal.(print_string [ green ] "Press 1, 2, 3, or 4:\n")
  in
  match read_line () with
  | "1" -> [| yellow |]
  | "2" -> [| yellow; red |]
  | "3" -> [| yellow; blue; red |]
  | "4" -> [| yellow; blue; red; green |]
  | _ ->
      let _ =
        ANSITerminal.(
          print_string [ red ] "\nInavlid command. Try again:\n")
      in
      init_players ()

let add_players_to_board players board num_players =
  let add_piece piece_num color piece =
    let row, col = piece.loc in
    match board.(row).(col) with
    | Space s ->
        board.(row).(col) <-
          Space { s with contents = Some (color, piece_num) }
    | Ignore -> failwith "impossible"
    | Die -> failwith "impossible"
  in
  for i = 0 to num_players - 1 do
    let player = players.(i) in
    for j = 0 to Array.length player.pieces - 1 do
      add_piece j player.color player.pieces.(j)
    done
  done;
  board

let init_game () =
  let players = init_players () in
  let num_players = Array.length players in
  let board = init_board () in
  let board = add_players_to_board players board num_players in
  { board; players; die_val = 0; turn = 0; num_players }
