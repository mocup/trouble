open Array
open ANSITerminal
open Random
open Specs

type color =
  | Red
  | Green
  | Blue
  | Yellow
(** Type [color] represents the color of a player. It can either be
    Red, Green, Blue, or Yellow. There will be at most one player
    of each color in a game.*)

type piece = {
  loc : int * int;
  start : int * int;
}
(** Type [piece] represents one piece for one player. It has current
    location [loc], and start zone location [start].*)

type player = {
  color : color;
  pieces : piece array;
}
(** Type [player] represents a player in a trouble game. Each player has
    color [color] and pieces [pieces].*)

type space = {
  contents : (color * int) option;
  next : (int * int) option;
  end_next : (int * int) option;
}
(** Type [space] represents a space on the board. A space contains
    [contents] which is either a colored piece or nothing. It has next
    space [next], and also next end zone space [end_next] if it is a
    space connecting to an end zone space.*)

type coord =
  | Ignore
  | Space of space
  | Die
(** Type [coord] represents a coordinate of a location on the
    board. Unused board locations have type [Ignore], locations
    pieces can use have type [Space], and the singular die
    location has type [Die].*)

type board = coord array array
(** Type [board] represents the full game board of coordinates.*)

type game = {
  board : board;
  players : player array;
  die_val : int;
  turn : int;
  num_players : int;
}
(** Type [game] represents the state of the game during each turn. It
    contains the board, players, die roll value, turn number, and number
    of players.*)

exception EndBoard
(**[EndBoard] is raised when a piece attempts to move past the end of
   its designated end zone.*)

val roll_die_val : unit -> int
(**[roll_die_val ()] returns a random number between 1 and 6.*)

val roll_die : color -> int
(** [roll_die color] is the value of a die roll prompted by player
    [color].*)

val in_start_zone : piece -> bool
(** [in_start_zone piece] returns [true] if [piece] is currently in its
    start zone space, and [false] otherwise.*)

val is_top_left : int -> int -> bool
(** [is_top_left row col] returns [true] if coordinates [(row, col)] are
    in the top left board quadrant, and [false] otherwise.*)

val is_top_right : int -> int -> bool
(** [is_top_right row col] returns [true] if coordinates [(row, col)]
    are in the top right board quadrant, and [false] otherwise.*)

val is_bottom_left : int -> int -> bool
(** [is_bottom_left row col] returns [true] if coordinates [(row, col)]
    are in the bottom left board quadrant, and [false] otherwise.*)

val is_bottom_right : int -> int -> bool
(** [is_bottom_right row col] returns [true] if coordinates [(row, col)]
    are in the bottom right board quadrant, and [false] otherwise.*)

val is_top_left_end_zone : int -> int -> bool
(** [is_top_left_end_zone row col] returns [true] if coordinates
    [(row,
    col)] are in the top left end zone, and [false]
    otherwise.*)

val is_top_right_end_zone : int -> int -> bool
(** [is_top_right_end_zone row col] returns [true] if coordinates
    [(row, col)] are in the top right end zone, and [false]
    otherwise.*)

val is_bottom_left_end_zone : int -> int -> bool
(** [is_bottom_left_end_zone row col] returns [true] if coordinates
    [(row, col)] are in the bottom left end zone, and [false] otherwise.*)

val is_bottom_right_end_zone : int -> int -> bool
(** [is_bottom_right_end_zone row col] returns [true] if coordinates
    [(row, col)] are in the bottom right end zone, and [false]
    otherwise.*)

val in_end_zone : int * int -> bool
(** [in_end_zone (row, col)] returns [true] if coordinates [(row, col)]
    specify an end zone, and [false] otherwise*)

val end_zone_reachable : color -> int -> int -> bool
(** [end_zone_reachable color row col] returns [true] if coordinates
    [(row, col)] lie within the quadrant containing player [color]'s
    start zone, and [false] otherwise.*)

val get_next_space : game -> int * int -> int -> int * int
(** [get_next_space game current_space moves] is the space reached after
    advancing [moves] times on [game.board] starting at [current_space].
    [EndBoard] is raised when a piece attempts to move past the end of
    its designated end zone.*)

val move_is_invalid : piece -> int -> game -> bool
(** [move_is_invalid piece die_val game] returns [true] if [piece] is
    attempting an invalid move, and [false] otherwise.*)

val choose_valid_piece : player -> int -> game -> int
(** [choose_valid_piece player die_val game] returns the index of the
    piece from player [player] selected by the user using
    [choose_piece]. The piece selected must be among the pieces that
    have a valid movee, otherwise an invalid move string is printed and
    [choose_valid_piece] is recursively called.*)

val no_valid_moves : player -> game -> bool
(** [no_valid_moves player game] returns [true] if [player] does not
    have any pieces that can be moved on the board die value [die_val],
    and [false] otherwise. *)

val move_from_start : game -> int -> game
(* [move_piece game piece_num] returns the updated game state after
   moving piece [piece_num] from the start zone and on to the active
   game board representing [game].*)

val move_piece : game -> int -> game
(** [move_piece game piece_num] is the updated game state after moving
    piece [piece_num] for one player after a die roll starting in state
    [game].*)

val update_turn : int -> int -> int
(** [update_turn turn num_players] returns the int corresponding to the
    next player after the player corresponding to [turn].*)

val check_for_winner : board -> color option
(** [check_for_winner board] returns [Some color] if player [color] has
    all four pieces in their end zone, and [None] otherwise.*)

val thanks_for_playing : color -> unit
(** [thanks_for_playing color] prints to the terminal the player who
    won.*)
