open State

val welcome : unit -> unit
(**[welcome ()] displays welcome messages and allows users to configure
   their game setup.*)

val is_end_of_end_zone : int -> int -> bool
(** [is_end_of_end_zone row col] returns [true] if coordinates
    [(row), (col)] are the last space in one of the four end zones, and
    [false] otherwise.*)

val init_board : unit -> board
(** [init_board ()] initalizes the board's start zones, end zones, and
    middle spaces. *)

val make_piece : int * int -> int * int -> piece
(**[make_piece (loc_x, loc_y) (start_x, start_y)] returns a piece with
   location [(loc_x, loc_y)] and starting position [(start_x, start_y)].*)

val add_players_to_board :
  player array -> coord array array -> int -> coord array array
(** [add_players_to_board players board num_players] adds four pieces to
    the board for each player up to [num_players].*)

val init_game : unit -> game
(**[init_game ()] returns a game in its default initialized state with
   all pieces starting in the start zones.*)
