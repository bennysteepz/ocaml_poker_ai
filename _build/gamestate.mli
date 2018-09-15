open Player
open Command

(* [stage] is a variant that indicates the current stage for each round.
   Deal stage is the pre-flop stage when public cards have not been
   shown, Flop is when the first 3 cards are shown, turn is when the
   fourth card is shown, and River is when all five cards are shown.
   Showdown is when the round has ended and a winner should be calculated,
   whereas empty is aa intermediate helper stage for the gui to show the
   winners before moving on to the next init stage. *)
type stage =
  | Deal
  | Flop
  | Turn
  | River
  | Showdown
  | Empty

(* [state] is an abstract class representing the state of the game. *)
type state = {
  (* list of all the players left in this round, small blind is always
     the first. When stage = deal, the order will instead be first player after
     big blind, ...., dealer, small blind, big blind. the order will be updated
     accordingly once a player raises. When order is empty, the state goes
     to the next stage. *)
  order : player list;
  (* list of all the players that are still alive in this round in order, with
     small blind as the head. *)
  alive : player list;
  players : player list; (* list of all the players *)
  pot : int;
  currentplayer : string;
  public_hand : card list;
  stage : stage; (* stage can be deal, flop, turn, river, showdown *)
  deck : card list;
  dealer: string;
  smallblind : string;
  bigblind : string;
  message : string;
  last_raise : int;
  last_bet : int;
  all_in : bool;
  (* stores the previous bets of each player during this stage.
     In the beginning of each stage, prev_bets is set ot an empty list. *)
  prev_bets : (string * int) list;
}

(* [overallstate] indicates the state of the whole game. *)
type overallstate =
  |Menu of state
  |Game of state
  |Quit

(* [hand] is a variant for the different types of games in poker. *)
type hand = High_Card | Pair | Two_Pair | Three_Kind | Straight | Flush |
  Full_House | Four_Kind | Straight_Flush | Royal_Flush

(* [init_blind lst] is a list of players with the order
 * dealer, small blind, big blind,... that stands for the initial queue.
 * The order is decided by random.
 * requires: [lst] is the list of all the players *)
val init_queue : player list -> player list

(* [init_state j] is the initial state of the game as
 * determined by player list [j].
 * requires: [j] is the list of all the players *)
val init_state : player list -> state

(* [current_player st] is the player id of the current  *)
val current_player : state -> string

(*[getplayer p id] returns the player in player list [p] that has name [id].*)
val getplayer : player list -> string -> player

(* [current_player_type st] returns the AI type of the current player.*)
val current_player_type : state -> ai_type

(* [players st] returns a list of the players *)
val players : state -> player list

(* [pot st] is the current total amount of money in the pot *)
val pot : state -> int

(* [small_blind st] is the current small blind player*)
val small_blind : state -> player

(* [big_blind st] is the current big blind player*)
val big_blind : state -> player

(* [dealer st] is the current dealer *)
val dealer : state -> player

(* [stage st] is the current stage of the game. The stage could be deal,
   flop, turn, river, or showdown. *)
val stage : state -> stage

(* [alive st] are the list of players still alive for this round. *)
val alive : state -> player list

(* [min_raise st] is the current minimum raise *)
val min_raise : state -> int

(* [public_hand st] is a list of all the cards currently on the table.
   The order of the list is essential, and follows the order that the cards
   were dealt on the table. There can only be a maximum of five cards in the
   list. *)
val public_hand : state -> card list

(* [deck st] returns the current deck *)
val deck : state -> card list

(* [queue st] is a list of players, in the order of the current queue. *)
val queue : state -> player list

(* [next_player st] is the player ID of the next  *)
val next_player : state -> string

(* [largest_bet st] is the largest bet made in the current round *)
val largest_bet : state -> int

(* [show_message st] is the string that should be shown on the terminal *)
val show_message : state -> string

(* [prev_bets st] returns the list that indicates how much each player
   has put in the pot during the current stage. *)
val prev_bets : state -> (string * int) list

(* [random_card st] returns a random card from the deck in [st], and the new state.
   requires: [st] is the current game state. *)
val random_card : state -> card * state

(*[best_hand cards] takes in a list of seven cards, containing both the public
  * cards and one player's cards and returns a tuple containing type hand and
  a list of each card's 'num' or values (ex: 7,7,4,3). This list is sorted from
  highest mode to lowest and highest value to lowest*)
val best_hand : card list -> (hand * card list)

(*[winner state] takes in a state in which the round has ended and returns the
 * player who has won that round. The winner of each round is determined
 * through comparing the [best_hand] of each player, and determining which
 * player has the highest-ranking hand.*)
val winner : state -> player list

(* [to_call st p] returns the amount of money player [p] needs to put down
   in order to call the current bet. *)
val to_call : state -> player -> int

(*[find_winners st] takes in a state in which the round has ended and returns
  a [state * bool] tuple. The state returned is the same as the input state,
  except with a message denoting who has won the round. The boolean indicates
  whether the players' hands in the round should be shown.*)
val win_state : state -> state * bool

(* [end' st] updates the state after the current round has ended. It should
   calculate who the winner is, and update the players balance accordingly.
   The returned state is the new init state for the next round. *)
val end' : state -> state

(* [do' c] is the new state after the command [c]. Command [c] should give
   information on whether the current player call, raise, fold, or checks.
   If the command is not permitted (ex: betting below the min bet), the state
   does not update any of its fields except for show_message, which should show
   an error message that prompts the same user for a valid command. *)
val do' : command -> state -> state


(*==========================Testing Purposes==================================*)
(* calculate score little functions for testing *)
val getIntList : Player.card list -> int list -> int list

val sortCards : Player.card list -> int list -> Player.card list ->
  Player.card list

val removeCard : Player.card list -> Player.card -> Player.card list ->
  Player.card list

val is_member : Player.card list -> Player.card -> bool

val trimList : Player.card list -> Player.card list -> Player.card list ->
  Player.card list

val maxFive : Player.card list -> Player.card list -> int -> Player.card list

val findHighCard : Player.card list -> Player.card list

val findPair : Player.card list -> Player.card list -> Player.card list ->
  Player.card list

val findTwoPair : Player.card list -> Player.card list -> Player.card list ->
  Player.card list -> Player.card list

val findThreeKind : Player.card list -> Player.card list -> Player.card list

val findStraight : Player.card list -> Player.card list -> Player.card list

val findFlush : Player.card list -> Player.card list -> Player.card list ->
  Player.card list -> Player.card list -> Player.card list

val findFullHouse : Player.card list -> Player.card list -> Player.card list

val findFourKind : Player.card list -> Player.card list -> Player.card list ->
  Player.card list

val findStraightFlush : Player.card list -> Player.card list -> Player.card list

val findRoyalFlush : Player.card list -> Player.card list
