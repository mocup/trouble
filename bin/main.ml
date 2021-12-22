open Game
open State
open Display
open Initialize

let get_current_player game = game.players.(game.turn)

let rec loop game =
  let winner = check_for_winner game.board in
  if winner <> None then
    match winner with
    | Some winner_color -> thanks_for_playing winner_color
    | None -> failwith "impossible"
  else
    let current_player = get_current_player game in
    let game = { game with die_val = roll_die current_player.color } in
    print_board game.board game.die_val;
    let game =
      if no_valid_moves current_player game then game
      else
        let piece_num =
          choose_valid_piece current_player game.die_val game
        in
        if in_start_zone current_player.pieces.(piece_num) then
          move_from_start game piece_num
        else move_piece game piece_num
    in
    print_board game.board game.die_val;
    let next_turn =
      if game.die_val = 6 then game.turn
      else update_turn game.turn game.num_players
    in
    let game = { game with turn = next_turn } in
    loop game

let () =
  welcome ();
  let new_game = init_game () in
  print_board new_game.board new_game.die_val;
  loop new_game
