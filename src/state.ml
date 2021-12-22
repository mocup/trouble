open Array
open ANSITerminal
open Random
open Specs

type color =
  | Red
  | Green
  | Blue
  | Yellow

type piece = {
  loc : int * int;
  start : int * int;
}

type player = {
  color : color;
  pieces : piece array;
}

type space = {
  contents : (color * int) option;
  next : (int * int) option;
  end_next : (int * int) option;
}

type coord =
  | Ignore
  | Space of space
  | Die

type board = coord array array

type game = {
  board : board;
  players : player array;
  die_val : int;
  turn : int;
  num_players : int;
}

exception EndBoard

(** [get_color_string color] returns the string representation of
    [color].*)
let get_color_string color =
  match color with
  | Red -> "Red"
  | Green -> "Green"
  | Blue -> "Blue"
  | Yellow -> "Yellow"

let roll_die_val () = Random.int 6 + 1

let rec roll_die color =
  let player = get_color_string color in
  ANSITerminal.(
    print_string [ white ]
      ("\nPlayer " ^ player ^ ", it is your turn.\n"));
  ANSITerminal.(print_string [ green ] "Press D to roll the die: \n");
  Random.self_init ();
  let input = read_line () in
  if input = "d" || input = "D" then roll_die_val ()
  else
    let _ =
      ANSITerminal.(
        print_string [ red ] "\nInvalid command. Try again:\n")
    in
    roll_die color

let in_start_zone piece = piece.loc = piece.start

let is_top_left row col = row < middle && col < middle

let is_top_right row col = row < middle && col > middle

let is_bottom_left row col = row > middle && col < middle

let is_bottom_right row col = row > middle && col > middle

let is_top_left_end_zone row col = row = col && is_top_left row col

let is_top_right_end_zone row col =
  row + col = end_start_zone_index && is_top_right row col

let is_bottom_left_end_zone row col =
  row + col = end_start_zone_index && is_bottom_left row col

let is_bottom_right_end_zone row col =
  row = col && is_bottom_right row col

let in_end_zone (row, col) =
  is_top_left_end_zone row col
  || is_top_right_end_zone row col
  || is_bottom_left_end_zone row col
  || is_bottom_right_end_zone row col

let end_zone_reachable color row col =
  (is_top_left row col && color = Yellow)
  || (is_top_right row col && color = Blue)
  || (is_bottom_left row col && color = Green)
  || (is_bottom_right row col && color = Red)

(** [get_space board row col] returns the contents of the space at
    [(row, col)] on the board.*)
let get_space board row col =
  match board.(row).(col) with
  | Space s -> s
  | _ -> failwith "impossible"

let rec get_next_space game current_space moves =
  let cur_row, cur_col = current_space in
  let space_contents = get_space game.board cur_row cur_col in
  match (moves, space_contents) with
  | 0, _ -> current_space
  | _, { end_next = Some (row, col) }
    when end_zone_reachable game.players.(game.turn).color row col ->
      get_next_space game (row, col) (moves - 1)
  | _, { next = Some (row, col) } ->
      get_next_space game (row, col) (moves - 1)
  | _ -> raise EndBoard

(** [own_piece_ko_start_zone piece die_val game color] returns [true] if
    [die_val] is 6 and [piece] in the start zone attempts to enter the
    board when there is already another piece of the same color [color]
    in that start zone exit space, and [false] otherwise.*)
let own_piece_ko_start_zone piece die_val game color =
  in_start_zone piece && die_val = 6
  &&
  let row, col = get_next_space game piece.start 1 in
  match (get_space game.board row col).contents with
  | Some (c, _) -> color = c
  | None -> false

(** [own_piece_ko die_val game row col color] returns [true] if a piece
    attempts to move [die_val] spaces to a new space from [(row, col)]
    where there is already a piece of the same color [color], and
    [false] otherwise.*)
let own_piece_ko die_val game row col color =
  let next_space = get_next_space game (row, col) die_val in
  match
    (get_space game.board (fst next_space) (snd next_space)).contents
  with
  | Some (c, _) -> color = c
  | None -> false

(**[reaches_end_of_board game row col die_val] returns [true] if a piece
   currently located at [(row, col)] attempts to move [die_val] spaces
   and reaches the end of its end zone, and [false] otherwise.*)
let reaches_end_of_board game row col die_val =
  match get_next_space game (row, col) die_val with
  | exception EndBoard -> true
  | _ -> false

let move_is_invalid piece die_val game =
  (in_start_zone piece && die_val <> 6)
  || own_piece_ko_start_zone piece die_val game
       game.players.(game.turn).color
  || reaches_end_of_board game (fst piece.loc) (snd piece.loc) die_val
  || (not (in_start_zone piece))
     && own_piece_ko die_val game (fst piece.loc) (snd piece.loc)
          game.players.(game.turn).color

let rec choose_valid_piece player die_val game =
  let rec choose_piece () =
    let _ =
      ANSITerminal.(
        print_string [ white ] "\nWhich piece would you like to move?\n");
      ANSITerminal.(print_string [ green ] "Press A, B, C, or D:\n")
    in
    match read_line () with
    | "a" -> 0
    | "A" -> 0
    | "b" -> 1
    | "B" -> 1
    | "c" -> 2
    | "C" -> 2
    | "d" -> 3
    | "D" -> 3
    | _ ->
        ANSITerminal.(
          print_string [ red ] "\nInvalid command. Try again:\n");
        choose_piece ()
  in
  let piece_num = choose_piece () in
  if move_is_invalid player.pieces.(piece_num) die_val game then
    let _ =
      ANSITerminal.(print_string [ red ] "\nInvalid move. Try again:\n")
    in
    choose_valid_piece player die_val game
  else piece_num

(** [check_for_any_valid_moves player game] returns [true] if none of
    the current player's pieces have a valid move, and [false]
    otherwise.*)
let rec check_for_any_valid_moves player game =
  let die_roll = game.die_val in
  let piece0 = player.pieces.(0) in
  let piece1 = player.pieces.(1) in
  let piece2 = player.pieces.(2) in
  let piece3 = player.pieces.(3) in
  move_is_invalid piece0 die_roll game
  && move_is_invalid piece1 die_roll game
  && move_is_invalid piece2 die_roll game
  && move_is_invalid piece3 die_roll game

let rec no_valid_moves player game =
  if check_for_any_valid_moves player game then
    let _ =
      ANSITerminal.(
        print_string [ white ]
          "No moves possible. Press C to continue:\n")
    in
    if read_line () = "c" then true
    else
      let _ =
        ANSITerminal.(
          print_string [ red ] "\nInvalid command. Try again:\n")
      in
      no_valid_moves player game
  else false

(** [get_ko_player_num ko_color players num_players] returns the int
    corresponding to the player number that is being knocked out of the
    space, and [-1] otherwise.*)
let get_ko_player_num ko_color players num_players =
  let player_num = ref (-1) in
  for x = 0 to num_players - 1 do
    if players.(x).color = ko_color then player_num := x else ()
  done;
  !player_num

(** [get_ko_piece_num row col pieces] returns the int corresponding to
    the piece that is being knocked out of the space, [-1] otherwise if
    no other piece is on the space*)
let get_ko_piece_num row col pieces =
  let piece_num = ref (-1) in
  for x = 0 to num_pieces - 1 do
    if pieces.(x).loc = (row, col) then piece_num := x else ()
  done;
  !piece_num

(** [update_piece_loc game player_num piece_num row col] updates piece
    [piece_num] belonging to player [player_num] to have location
    [(row), (col)].*)
let update_piece_loc game player_num piece_num row col =
  game.players.(player_num).pieces.(piece_num) <-
    {
      (game.players.(player_num).pieces.(piece_num)) with
      loc = (row, col);
    }

(** [update_space_contents game row col contents] updates the contents
    of the space at [(row), (col)] with [contents]*)
let update_space_contents game row col contents =
  game.board.(row).(col) <-
    Space { (get_space game.board row col) with contents }

(** [knockout ko_color next_row next_col game] removes the piece
    currently at [(next_row), (next_col)] and places it back in the
    start zone designated for [ko_color]*)
let knockout ko_color next_row next_col game =
  ANSITerminal.(print_string [ red ] "\nGet knocked out!\n");
  let ko_player_num =
    get_ko_player_num ko_color game.players game.num_players
  in
  let ko_player = game.players.(ko_player_num) in
  let ko_piece_num =
    get_ko_piece_num next_row next_col ko_player.pieces
  in
  let start_row, start_col = ko_player.pieces.(ko_piece_num).start in
  update_piece_loc game ko_player_num ko_piece_num start_row start_col;
  update_space_contents game start_row start_col
    (Some (ko_color, ko_piece_num));
  update_space_contents game next_row next_col None

(** [extract_coords coord_option] returns the contents [(row), (col)] of
    [Some (row,col)], and fails if coord_option is [None]*)
let extract_coords = function
  | Some (row, col) -> (row, col)
  | None -> failwith "impossible"

let rec move_from_start game piece_num =
  let player = game.players.(game.turn) in
  let start_row, start_col = player.pieces.(piece_num).start in
  let cur_space = get_space game.board start_row start_col in
  update_space_contents game start_row start_col None;
  let next_row, next_col = extract_coords cur_space.next in
  let next_space = get_space game.board next_row next_col in
  (match next_space.contents with
  | None -> ()
  | Some (ko_color, ko_piece_num) ->
      knockout ko_color next_row next_col game);
  update_space_contents game next_row next_col
    (Some (player.color, piece_num));
  update_piece_loc game game.turn piece_num next_row next_col;
  game

let move_piece game piece_num =
  let player = game.players.(game.turn) in
  let cur_row, cur_col = player.pieces.(piece_num).loc in
  update_space_contents game cur_row cur_col None;
  let next_row, next_col =
    get_next_space game (cur_row, cur_col) game.die_val
  in
  let next_space = get_space game.board next_row next_col in
  (match next_space.contents with
  | None -> ()
  | Some (ko_color, ko_piece_num) ->
      knockout ko_color next_row next_col game);
  update_space_contents game next_row next_col
    (Some (player.color, piece_num));
  update_piece_loc game game.turn piece_num next_row next_col;
  game

let update_turn turn num_players = (turn + 1) mod num_players

(**[end_zone_full board row col] returns [true] if the end zone starting
   at coordinates [(row), (col)] is full, and [false] otherwise.*)
let rec end_zone_full board row col =
  let space = get_space board row col in
  if space.contents = None then false
  else
    let next_space = space.next in
    if next_space = None then true
    else
      let next_row, next_col = extract_coords next_space in
      end_zone_full board next_row next_col

let check_for_winner board =
  if end_zone_full board (start_space_index + 1) (start_space_index + 1)
  then Some Yellow
  else if
    end_zone_full board (start_space_index + 1) (end_space_index - 1)
  then Some Blue
  else if
    end_zone_full board (end_space_index - 1) (start_space_index + 1)
  then Some Green
  else if end_zone_full board (end_space_index - 1) (end_space_index - 1)
  then Some Red
  else None

let rec thanks_for_playing color =
  ANSITerminal.(
    print_string [ white ]
      ("\nCongratulations Player " ^ get_color_string color ^ "\n");
    ANSITerminal.(print_string [ white ] "YOU WON!\n"))
