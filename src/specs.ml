(* Total length of the board *)
let board_length = 15

(* Length of square pieces traverse, aka the inner board*)
let inner_length = 11

(* Index of the first start zone location*)
let start_zone_index = 0

(* Index of the last start zone location *)
let end_start_zone_index = board_length - 1

(* Index of the first row/column of the inner board *)
let start_space_index = (board_length - inner_length) / 2

(* Index of the last row/column of the inner board*)
let end_space_index = start_space_index + inner_length - 1

(* Middle of the board *)
let middle = (board_length - 1) / 2

(* Number of pieces per player*)
let num_pieces = 4
