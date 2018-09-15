open Notty
open Gamestate

(* recursively iterates until the player either starts the game or exits the
   menu. Draws the menu_items and displays them on the terminal, then waits
   for a player response. If the response is enter, depending on at what button
   the player is at, it will draw different outputs. *)
val menu : Notty_unix.Term.t -> state -> unit
