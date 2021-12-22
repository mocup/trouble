open OUnit2
open Game
open State
open Initialize
open Specs

let roll_die_test
    (name : string)
    (expected_min : int)
    (expected_max : int) : test =
  name >:: fun _ ->
  assert_equal true
    (expected_min <= roll_die_val () && expected_max >= roll_die_val ())

let roll_die_tests =
  [
    roll_die_test "Roll die random value 1" 1 6;
    roll_die_test "Roll die random value 2" 1 6;
    roll_die_test "Roll die random value 3" 1 6;
    roll_die_test "Roll die random value 4" 1 6;
  ]

let in_start_zone_test
    (name : string)
    (piece : piece)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (in_start_zone piece)

let in_end_zone_test
    (name : string)
    (row : int)
    (col : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (in_end_zone (row, col))

let end_zone_reachable_test
    (name : string)
    (color : color)
    (row : int)
    (col : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (end_zone_reachable color row col)

let location_tests =
  [
    in_start_zone_test "in start zone"
      (make_piece (10, 10) (10, 10))
      true;
    in_start_zone_test "not in start zone"
      (make_piece (10, 10) (10, 9))
      false;
    in_end_zone_test "in top left end zone" 6 6 true;
    in_end_zone_test "in top right end zone" 5 9 true;
    in_end_zone_test "in bottom left end zone" 9 5 true;
    in_end_zone_test "in bottom right end zone" 11 11 true;
    end_zone_reachable_test "yellow can reach yellow's end zone" Yellow
      4 4 true;
    end_zone_reachable_test "yellow can't reach red's end zone" Yellow
      10 10 false;
    end_zone_reachable_test "yellow can't reach yellow's end zone"
      Yellow 0 10 false;
  ]

let make_custom_player c ps = { color = c; pieces = ps }

let make_one_player_game (start_row, start_col) (cur_row, cur_col) =
  let y_pieces =
    [| make_piece (cur_row, cur_col) (start_row, start_col) |]
  in
  let y_player = make_custom_player Yellow y_pieces in
  let players = [| y_player |] in
  let num_players = Array.length players in
  let board =
    add_players_to_board players (init_board ()) num_players
  in
  { board; players; die_val = 0; turn = 0; num_players }

let get_next_space_test
    (name : string)
    (game : game)
    (current_space : int * int)
    (moves : int)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_next_space game current_space moves)

let get_next_space_tests =
  [
    get_next_space_test "move 0 spaces around board"
      (make_one_player_game (3, 0) (2, 3))
      (2, 3) 0 (2, 3);
    get_next_space_test "move 1 space around board"
      (make_one_player_game (3, 0) (2, 3))
      (2, 3) 1 (2, 4);
    get_next_space_test "move 6 spaces around board"
      (make_one_player_game (3, 0) (2, 3))
      (2, 3) 6 (2, 9);
    get_next_space_test "move 5 spaces into end zone"
      (make_one_player_game (3, 0) (5, 2))
      (5, 2) 5 (5, 5);
    get_next_space_test "move into yellow end zone from green quadrant"
      (make_one_player_game (3, 0) (5, 2))
      (8, 2) 6 (3, 3);
    get_next_space_test "yellow moves past blue start zone"
      (make_one_player_game (3, 0) (2, 9))
      (2, 9) 4 (4, 12);
    get_next_space_test "move within end zone"
      (make_one_player_game (3, 0) (5, 5))
      (5, 5) 1 (6, 6);
  ]

let make_one_player_game_two_pieces
    (start_row_1, start_col_1)
    (cur_row_1, cur_col_1)
    (start_row_2, start_col_2)
    (cur_row_2, cur_col_2) =
  let y_pieces =
    [|
      make_piece (cur_row_1, cur_col_1) (start_row_1, start_col_1);
      make_piece (cur_row_2, cur_col_2) (start_row_2, start_col_2);
    |]
  in
  let y_player = make_custom_player Yellow y_pieces in
  let players = [| y_player |] in
  let num_players = Array.length players in
  let board =
    add_players_to_board players (init_board ()) num_players
  in
  { board; players; die_val = 0; turn = 0; num_players }

let make_two_player_game
    (p1_start_row, p1_start_col)
    (p1_cur_row, p1_cur_col)
    (p2_start_row, p2_start_col)
    (p2_cur_row, p2_cur_col) =
  let y_pieces =
    [|
      make_piece (p1_cur_row, p1_cur_col) (p1_start_row, p1_start_col);
    |]
  in
  let y_player = make_custom_player Yellow y_pieces in
  let r_pieces =
    [|
      make_piece (p2_cur_row, p2_cur_col) (p2_start_row, p2_start_col);
    |]
  in
  let r_player = make_custom_player Red r_pieces in
  let players = [| y_player; r_player |] in
  let num_players = Array.length players in
  let board =
    add_players_to_board players (init_board ()) num_players
  in
  { board; players; die_val = 0; turn = 0; num_players }

let invalid_move_test
    (name : string)
    (piece : piece)
    (die_val : int)
    (game : game)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (move_is_invalid piece die_val game)

let invalid_move_tests =
  [
    invalid_move_test "yellow tries to knock itself out from start zone"
      (make_piece (3, 0) (3, 0))
      6
      (make_one_player_game_two_pieces (3, 0) (3, 0) (2, 1) (2, 3))
      true;
    invalid_move_test
      "yellow doesn't try to knock itself out from start zone"
      (make_piece (3, 0) (3, 0))
      6
      (make_one_player_game_two_pieces (3, 0) (3, 0) (2, 1) (2, 4))
      false;
    invalid_move_test "yellow didn't roll 6 and can't leave start zone"
      (make_piece (3, 0) (3, 0))
      5
      (make_one_player_game_two_pieces (3, 0) (3, 0) (2, 1) (3, 2))
      true;
    invalid_move_test "yellow tries to knock itself out on board"
      (make_piece (2, 3) (3, 0))
      3
      (make_one_player_game_two_pieces (3, 0) (2, 3) (2, 1) (2, 6))
      true;
    invalid_move_test "yellow doesn't try to knock itself out on board"
      (make_piece (2, 3) (3, 0))
      4
      (make_one_player_game_two_pieces (3, 0) (2, 3) (2, 1) (2, 6))
      false;
    invalid_move_test "yellow tries to knock itself out in end zone"
      (make_piece (5, 2) (3, 0))
      6
      (make_one_player_game_two_pieces (3, 0) (5, 2) (2, 1) (6, 6))
      true;
    invalid_move_test "end zone valid move"
      (make_piece (11, 11) (3, 0))
      3
      (make_one_player_game (3, 0) (11, 11))
      false;
    invalid_move_test "end zone invalid move"
      (make_piece (11, 11) (3, 0))
      4
      (make_one_player_game (3, 0) (11, 11))
      true;
  ]

let suite =
  "test suite for trouble final project"
  >::: List.flatten
         [
           roll_die_tests;
           location_tests;
           get_next_space_tests;
           invalid_move_tests;
         ]

let _ = run_test_tt_main suite