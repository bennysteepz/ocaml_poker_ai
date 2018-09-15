(*stores the command that the player is executing*)
type command =
  | Raise of int
  | Call
  | Fold
  | Show
  | Status
  | Manual
  | None
  | Exit

(*[parse s] takes in a string [s] and converts it into a command.*)
val parse : string -> command
