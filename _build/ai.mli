(* [easy_move st p] returns a command for the easy AI given state [st].
   - requires: [st] is a valid state and [p] is an easy AI player. *)
val easy_move : Gamestate.state -> Player.player -> Command.command

(* [medium_move st p] returns a command for the medium AI given state [st].
   - requires: [st] is a valid state and [p] is an medium AI player. *)
val medium_move : Gamestate.state -> Player.player -> Command.command

(* [hard_move st p] returns a command for the hard AI given state [st].
   - requires: [st] is a valid state and [p] is an hard AI player. *)
val hard_move : Gamestate.state -> Player.player -> Command.command
