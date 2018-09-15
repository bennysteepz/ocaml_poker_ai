(*This interface will be the structure used for both the human player and
* the computer player modules.*)

(*suit describes the suit of a card.*)
type suit = Spade | Heart | Diamond | Club

type ai_type = None | Easy | Medium | Hard

(*Card is a record that contains a suit and a card number.*)
type card = {
  num : int;
  suit : suit;
}

(*Contains player information, including their name, balance, their current
 * hand in the game, and whether they are still in the current round.
   Also stores the fold/call probability for each player. *)
type player = {
  is_ai : ai_type;
  name : string;
  balance : int;
  hand : (card * card) option;
  folded : bool;
  all_in : bool;
  f_numer : int;
  denom : int;
  c_numer : int;
}

(*[init_player s] returns a player of type [t] with [name = s]*)
val init_player : string -> ai_type -> player

val player_type : player -> ai_type

(*[name p] returns the name of the player described in p.*)
  val name: player -> string

(*[balance p] returns the current balance player p has. This does not
 * include the money that [p] has placed in the pool in the current round.*)
val balance : player -> int


(* [all_in p] returns whether the player p has gone all_in or not. *)
val all_in : player -> bool

(* [set_all_in p b] sets the player's all_in field to b. *)
val set_all_in : player -> bool -> player

(*[folded p] returns true if [p] is still playing in the current round,
 * and false otherwise.*)
  val folded : player -> bool

(*[get_hand p] returns the hand that [p] is currently holding, if they are
 * still in the current round. If they are not in the current round,
 * [get_hand p] will return None.*)
val get_hand : player -> (card * card) option

(*[string_of_hand p] returns a string stating the hand that [p] is currently
 *holding. If they are not in the current round, this will return "None".*)
val string_of_hand : player -> string

(*[set_balance p i] changes the balance of the player and returns an updated
 * t with the updated balance. Input should be a value greater than or equal
 * to 0*)
val set_balance : player -> int -> player

(*[set_hand p h] changes the hand of the player and returns an updated t with
 * the updated hand. Input should be a pair of valid cards*)
val set_hand : player -> (card * card) -> player

(*[change_folded t] changes the state of folded to the opposite of what it was
 * before. If it was true, it's changed to false, if it's false then true. It
 * returns an updated t with the opposite boolean value*)
val change_folded : player -> player

(*[reset_player p] takes in a player after they have finished a game round,
  and returns that player with an empty hand, with the folded and all_in fields
  set to false, if they were not already false.*)
val reset_player : player -> player

(* [fold_prob p] returns the fold probability for player [p], which
   is calculated by p.f_numer / p.f_denom. *)
val fold_prob : player -> float


(* [call_prob p] returns the call probability for player [p], which
   is calculated by p.c_numer / p.c_denom. *)
val call_prob : player -> float

(* [raise_prob p] returns the raise probability for player [p], which
   is calculated by 1 - (fold_prob p) - (call_prob p) *)
val raise_prob : player -> float
