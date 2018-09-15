open Notty
open Gamestate

(* Takes in the current player index and the raise amount, the open terminal,
   and the current gamestate and loops recursively until the game ends or the
   player hits esc. This method should only be called from menu.ml after the
   player presses "start game" in the menu. *)
val gmain : int*int -> Notty_unix.Term.t -> state -> unit
