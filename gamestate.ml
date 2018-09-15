open Player
open Command

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

type overallstate =
  |Menu of state
  |Game of state
  |Quit

type hand = High_Card | Pair | Two_Pair | Three_Kind | Straight | Flush |
  Full_House | Four_Kind | Straight_Flush | Royal_Flush

(* [get_prevbet lst id] returns the previous bet of player name id
    during the current stage.
   - requires: [id] is a string of then anme of a player, and
              [lst] is a string * int tuple list. if [id] is not
              fount in lst, return 0. *)
let rec get_prevbet lst id =
  match lst with
  | [] -> 0
  | (n, i)::t ->
    if (n = id) then i else get_prevbet t id

(* helper function for updating prev_bets. *)
let rec new_prevbets lst id bet acc =
  match lst with
  | [] -> (id, bet)::acc
  | (n, i)::t ->
    if (n = id) then List.append (acc) ((n,i+bet)::t)
    else new_prevbets t id bet ((n,i)::acc)

(* [getplayer lst id] is the player with name = [id] in the [lst]
   - requires: [lst] is a list of players, [id] is a string
              [id] must be the name of one of the players in [lst].
   - raises Failure "Player Not Found" if there is no player with
              name = [id] in [lst] *)
let rec getplayer lst id =
  match lst with
  | [] -> raise (Failure "Player Not Found")
  | h::t ->
    if (name h = id) then h
    else getplayer t id

(* [remove lst id acc] removes the player with name = id from lst.
   order is restored. *)
let rec remove lst id acc =
  match lst with
  | [] -> acc
  | h::t ->
    if (name h = id) then List.append (List.rev acc) t
    else remove t id (h::acc)

(* [update lst p] is the new list with player that has the same name as p
   replaced in lst. order is restored. *)
let rec update lst p acc =
  match lst with
  | [] -> acc
  | h::t ->
    if (name h = name p) then List.append (List.rev acc) (p::t)
    else update t p (h::acc)

(* [next lst id] returns the next player *)
let rec next lst id  =
  let hd = List.hd lst in
  let rec loop lst id =
    match lst with
    | h::b::t ->
      if (name h = id) then b
      else loop (b::t@[h]) id
    | _ -> hd in
  loop lst id

(*Returns a list containing the 52 cards in a card deck.*)
let new_deck = fun () ->
  let rec add_card card lst =
    if card.num = 14
    then let new_lst = card :: lst in
      if card.suit = Diamond
      then add_card {suit = Club; num = 2} new_lst
      else if card.suit = Club
      then add_card {suit = Heart; num = 2} new_lst
      else if card.suit = Heart
      then add_card {suit = Spade; num = 2} new_lst
      else new_lst
    else add_card {card with num = card.num + 1} (card :: lst)
  in add_card {suit = Diamond; num = 2} []

(*[string_of_stage st] takes in a state and returns a string of the current
 * game stage.*)
let string_of_stage st =
  match st.stage with
  | Deal -> "Deal"
  | Flop -> "Flop"
  | Turn -> "Turn"
  | River -> "River"
  | Showdown -> "Showdown"
  | Empty -> ""

(*[string_of_public_hand st] takes in a state [st] and returns a string
 * containing the public hand of [st].*)
let string_of_public_hand st =
  let string_of_suit s =
    match s with
    | Spade -> "Spades"
    | Heart -> "Hearts"
    | Diamond -> "Diamonds"
    | Club -> "Clubs"
  in let string_of_num n =
     if n = 14 then "Ace"
     else if n = 11 then "Jack"
     else if n = 12 then "Queen"
     else if n = 13 then "King"
     else string_of_int n
  in let f acc s =
       acc ^ (string_of_num s.num) ^ " of " ^ (string_of_suit s.suit) ^ "\n             "
  in if st.public_hand = [] then "None" else List.fold_left f "" st.public_hand

(*[string_of_players st] takes in a state [st] and returns a string
 * containing the players still participating in the current round.*)
let string_of_players st =
  let f acc s = acc ^ s.name ^ "\n                           "
  in List.fold_left f "" st.alive

let string_of_order st =
  let f acc s = acc ^ s.name ^ "\n       "
  in List.fold_left f "" st.order

let string_of_balances st =
  let f acc s = acc ^ s.name ^ ": " ^ string_of_int s.balance ^ "\n                         "
  in List.fold_left f "" st.players

let string_of_prevbets st =
  let lst = st.prev_bets in
  let f acc (a,b) = acc ^ a ^ ": " ^ string_of_int b ^ "\n               "
  in List.fold_left f "" lst


(*[random_card st] takes in a state [st] and returns a tuple with a random
 *card from the deck in [st], and the new state with the chosen card
 * removed from the deck.*)
let random_card st =
  let d = st.deck in
  let i = Random.int (List.length d) in
  let c = List.nth d i in
  match List.partition (fun x -> x = c) d with
  | (card_drawn, new_deck) -> (c,{st with deck = new_deck})

(* [begin_deal st] updates a state after showing the flop. A card should
   be burned, and three cards should be added to the table. *)
let begin_deal st =
  let burn_state = random_card st in
  let first_card = random_card (snd burn_state) in
  let second_card = random_card (snd first_card) in
  let third_card = random_card (snd second_card) in
  let final_state = snd third_card in
  let p_hand = (fst first_card) :: (fst second_card) ::
               (third_card |> fst) :: []
  in {final_state with public_hand = p_hand}

(* [turn_deal st] updates a state after showing the turn or the river.
   A card should be burned, and the second card is added to the table.
 * requires: [st] only has 3 or 4 cards for public_hand. *)
let turn_deal st =
  let burn_state = st |> random_card |> snd in
  let d = burn_state |> random_card in
  let p_hand = st.public_hand @ [d |> fst]
  in {(d |> snd) with public_hand = p_hand}

(* [revove_card d c a] returns a new deck of cards with the [card]
   removed from [deck]. Order of the deck is not restored.*)
let rec remove_card deck card acc =
  match deck with
  | [] -> acc
  | h::t ->
    if (h.num = card.num && h.suit = card.suit) then
      List.append t acc
    else remove_card t card (h::acc)

(*[init_queue lst] takes in a list of player ids [lst] and returns a list with
 *the elements of [lst] scrambled.*)
let init_queue lst =
  let () = Random.init (int_of_float (Unix.time())) in
  let rec rearrange lst acc =
    if lst = [] then acc
    else let p = List.nth lst (Random.int (List.length lst)) in
      let f p' = p = p' in
      rearrange (snd (List.partition f lst)) (p :: acc)
  in rearrange lst []

(* [deal_to_players st] returns a new state with new cards delt to players. *)
let deal_to_players st =
  let f st p =
  let first_card = random_card st in
  let second_card = random_card (snd first_card) in
  let pl = set_hand p (fst first_card, fst second_card) in
  {st with order = update st.order pl [];
           alive = update st.alive pl [];
           players = update st.players pl [];
           deck = (snd second_card).deck}
  in List.fold_left f st st.alive

(*Takes in a state and rotates the order field of the state so that player
 * [new_hd] is now the first element in order. Returns the resulting state.
 * Requires: [new_hd] is in [st.order]*)
  let rec rotate_players new_hd st =
    if st.currentplayer = (new_hd).name
    then st
    else let new_order = List.tl (st.order) @ [List.hd (st.order)] in
      rotate_players new_hd
        {st with order = new_order;
                 currentplayer = (List.hd new_order).name}

  (*Takes in a state where the game has just begun, with the dealer being the
   * first element in [st.order]. Returns the state where the big blind and small
   * blind bet their blinds, and it is the next person's turn.*)
  let bet_blinds st =
    let bet num p st =
    let blind = getplayer st.alive p
    in let bal = blind.balance in
    if bal >= num
    then let blind' = set_balance blind (bal - num) in
      {st with order = update st.order blind' [];
               pot = st.pot + num;
               last_bet = num;
               last_raise = num;
               alive = update st.alive blind' [];
               players = update st.players blind' [];
               prev_bets = (blind'.name,num)::st.prev_bets}
    else let blind' = set_balance blind 0 in
      {st with order = update st.order blind' [];
               pot = st.pot + bal;
               last_bet = bal;
               last_raise = bal;
               alive = update st.alive blind' [];
               players = update st.players blind' [];
               all_in = true;
               prev_bets = (blind'.name, bal) :: st.prev_bets}
    in if List.length (st.alive) = 2
    then st |> bet 25 st.smallblind |> bet 50 st.bigblind
         |> rotate_players (getplayer st.players st.bigblind)
  else st |> bet 25 st.smallblind |> bet 50 st.bigblind
       |> rotate_players (next st.alive st.bigblind)

(*[init_state lst] takes in a list of player ids [lst] and returns a new state
 * with initialized players corresponding to each player id in [lst].*)
let init_state lst =
  let rec rotate_list new_hd lst =
    if List.hd lst = new_hd
    then lst
    else rotate_list new_hd (List.tl lst @ [List.hd lst])
  in let ps = init_queue lst in
  let dl = (List.hd ps).name in
  let small = if List.length lst = 2 then (List.hd ps)
    else (next ps dl) in
  let big = if List.length lst = 2 then (next ps dl)
    else (next ps small.name) in
  let st =
  {order  = ps;
  alive = rotate_list small ps;
  players = ps;
  pot = 0;
  currentplayer = dl;
  public_hand = [];
  stage = Deal;
  deck = new_deck ();
  dealer = dl;
  smallblind = small.name;
  bigblind = big.name;
  message = "";
  last_raise = 0;
  last_bet = 0;
  all_in = false;
  prev_bets = [];
} in st |> deal_to_players |> bet_blinds

(* [current_player st] is the id of the current player *)
let current_player st =
  st.currentplayer

(* [current_player_type st] is the type of the current player *)
let current_player_type st =
  player_type (getplayer st.players st.currentplayer)

(* [players st] is the list of all the players *)
let players st = st.players

(* [pot st] is the current pot *)
let pot st =
  st.pot

(* [small_blind st] is the current small blind player *)
let small_blind st =
  getplayer st.players st.smallblind

(* [big_blind st] is the current big blind player.  *)
let big_blind st =
  getplayer st.players st.bigblind

(* [dealer st] is the current dealer. *)
let dealer st =
  getplayer st.players st.dealer

(* [alive st] is the list of players that are still alive in this round,
   with the small blind as the first in list. *)
let alive st =
  st.alive

(* [stage st] returns the current stage *)
let stage st =
  st.stage

(* [public_hand st] returns the list of cards that indicate the current
   public cards on the table. *)
let public_hand st =
  st.public_hand

(* [queue st] returns the list of players that still have to make a
   move in the current stage by order. *)
let queue st =
  st.order

(* [next_player st] returns the id of the next player in line. *)
let next_player st =
  name (List.hd (st.order))

(* [largest_bet st] returns the largest bet for this stage. *)
let largest_bet st =
  st.last_bet

(* [deck st] returns the list of cards that are still in deck. *)
let deck st =
  st.deck

(* [min_raise st] returns the minimum raise for this stage. *)
let min_raise st =
  st.last_raise

(* [prev_bets st] returns a (id, int) list that keeps track of
   the previous bets in this stage for each player. *)
let prev_bets st =
  st.prev_bets

(* [show_message st] returns the message to be shown. *)
let show_message st =
  st.message

(* [to_call st p] returns the amount of money player [p] needs to put down
   in order to call the current bet.
   - requires: [st] is a gamestate, and [p] is a player that is still
               alive in [st]. *)
let to_call st p =
  let lst = prev_bets st in
  let rec find_p lst p =
    match lst with
    | [] -> largest_bet st
    | (a,b)::t ->
      if (a = name p) then (largest_bet st) - b
      else find_p t p in
  find_p lst p

(*============================Calculate_score=============================== *)
(* used in stabe_sort later *)
let compareCards i1 i2 =
  i1 - i2
(* helper function for sortCards - removes a card from a list of cards *)
let rec removeCard cards card acc =
  match cards with
    | [] -> acc
    | h::t ->
      if h.num = card.num && h.suit = card.suit
      then List.append t acc
      else removeCard t card (h::acc)
(* helper function for sortCards - takes in card list returns sorted int list
from greatest to smallest*)
let rec getIntList cards intList =
  match cards with
    | [] -> (List.rev (List.stable_sort compareCards intList))
    | h::t -> getIntList t ((h.num)::intList)
(* takes in card list and returns ordered card list from greatest to smallest.
pairs/trips/quads are in no particular order. *)
let rec sortCards cards intList sorted =
  match intList with
    | [] -> List.rev sorted
    | h::t ->
    let rec checkCards cardList =
      match cardList with
        | [] -> raise (Failure "Cards not matched with intList")
        | a::b ->
          if a.num = h then sortCards (removeCard cards a []) t (a::sorted)
          else checkCards b
    in
    checkCards cards
(* takes in card list and card, returns true if card in card list, else false *)
let rec is_member lst crd =
  match lst with
    | [] -> false
    | h::t -> if h = crd then true else is_member t crd
(* takes in a list and a sublist, returns list without sublist cards *)
let rec trimList lst sublst newlst =
  let sortedCards crdList = sortCards crdList (getIntList crdList []) [] in
  match lst with
    | [] -> sortedCards newlst
    | h::t ->
      let rec subLoop sub =
        match sub with
          | [] -> trimList t sublst (h::newlst)
          | a::b ->
            if h = a then trimList t (removeCard sublst a []) newlst
            else subLoop b
      in
      (subLoop sublst)
(* takes in card list, returns first five elements of that list if length > 5,
or list itself if length <= 5 *)
let rec maxFive lst newlst counter =
  match lst with
    | [] -> List.rev newlst
    | h::t ->
      if counter > 4 then List.rev newlst else maxFive t (h::newlst) (counter+1)
(* takes in ordered card list (greatest to smallest), returns first 5 cards *)
let rec findHighCard cards =
  maxFive cards [] 0
(* takes in ordered card list (greatest to smallest), returns pair followed by
next three highest cards in the card list or empty list if no pair in card list*)
let rec findPair cards hold original =
  match cards with
    | [] -> []
    | h::t ->
    if hold = [] then findPair t [h] original
    else if h.num = (List.hd hold).num
    then maxFive (List.append [h;(List.hd hold)]
      (trimList original [h;(List.hd hold)] [])) [] 0
    else findPair t [h] original
(* takes in ordered card list (greatest to smallest), returns highest pair
first, second highest pair second, and highest leftover card next in card list
or empty list if no two pair in card list.*)
let rec findTwoPair cards hold first original =
  match cards with
    | [] -> []
    | h::t ->
    match first with
      | [] ->
        (match hold with
          | [] -> findTwoPair t [h] [] original
          | [x] -> if h.num = x.num then findTwoPair t [] [h;x] original
            else findTwoPair t [h] [] original
          | _ -> raise(Failure "findtwopair has 3 of a kind error"))
      | _ ->
        (match hold with
          | [] -> findTwoPair t [h] first original
          | [x] -> if h.num = x.num
            then maxFive (List.append (List.append first [h;x])
              (trimList original (List.append first [h;x]) [])) [] 0
            else findTwoPair t [h] first original
          | _ -> raise(Failure "findtwopair has 3 of a kind error"))
(* takes in ordered card list (greatest to smallest), returns card list with
three of a kind cards in it - length of three or empty list if no trips *)
let rec findThreeKind cards hold =
  match cards with
    | [] -> []
    | h::t ->
    (match hold with
      | [] -> findThreeKind t (h::[])
      | x::[] -> if h.num = x.num then findThreeKind t (h::hold)
          else findThreeKind t (h::[])
      | a::b::[] -> if h.num = a.num
        then (h::hold)
        else findThreeKind t (h::[])
      | _ -> raise(Failure
        "findthreekind error shouldn't be matching with wildcard"))
(* takes in ordered card list (greatest to smallest), returns card list with
straight (or empty list if no straight in card list) *)
let rec findStraight cards hold =
  if List.length hold = 5 then List.rev hold else
  match cards with
    | [] -> []
    | h::t ->
      match hold with
        | [] -> findStraight t [h]
        | _ -> if h.num + 1 = (List.hd hold).num then findStraight t (h::hold)
            else findStraight t [h]
(* takes in ordered card list (greatest to smallest), returns card list with
flush ordered greatest to smallest (or empty list if no flush in card list) *)
let rec findFlush cards spades hearts diamonds clubs =
  match cards with
    | [] -> if List.length spades >= 5 then List.rev spades
        else if List.length hearts >= 5 then List.rev hearts
        else if List.length diamonds >= 5 then List.rev diamonds
        else if List.length clubs >= 5 then List.rev clubs
        else []
    | h::t ->
      match h.suit with
        | Spade -> findFlush t (h::spades) hearts diamonds clubs
        | Heart -> findFlush t spades (h::hearts) diamonds clubs
        | Diamond -> findFlush t spades hearts (h::diamonds) clubs
        | Club -> findFlush t spades hearts diamonds (h::clubs)
(* takes in ordered card list (greatest to smallest), returns card list of pair
if pair in card list,otherwise returns empty list(helper function for fullhouse)*)
let rec findPairHelper cards hold original =
  match cards with
    | [] -> []
    | h::t ->
    if hold = [] then findPairHelper t [h] original
    else if h.num = (List.hd hold).num
    then [h;(List.hd hold)] else findPairHelper t [h] original
(* takes in ordered card list (greatest to smallest), returns card list of
full house from highest pair or triple to lowest, or [] if no full house found *)
let rec findFullHouse cards hold =
  let rec delWhere lst numm newlst =
    match lst with
      | [] -> List.rev newlst
      | h::t -> if h.num = numm then delWhere t numm newlst
        else delWhere t numm (h::newlst)
  in
  let testThree = (findThreeKind cards []) in
  let numm = if testThree != [] then (List.hd testThree).num else 0 in
  let testTwo = (findPairHelper (delWhere cards numm []) [] (delWhere cards numm [])) in
  if testTwo != [] && testThree != [] then
    if List.hd testThree > List.hd testTwo
    then List.append testThree testTwo
    else List.append testTwo testThree
  else []
(* takes in ordered card list (greatest to smallest), returns card list of length
4 with four of a kind, or empty list if no quads are found *)
let rec findFourKind cards hold original =
  match cards with
    | [] -> []
    | h::t ->
    match hold with
      | [] -> findFourKind t [h] original
      | [x] -> if h.num = x.num then findFourKind t (h::hold) original
          else findFourKind t [h] original
      | [a;b] -> if h.num = b.num then findFourKind t (h::hold) original
          else findFourKind t [h] original
      | [a;b;c] -> if h.num = c.num
        then (h::hold)
        else []
      | _ -> raise(Failure "findFourKind shouldn't be running this wildcard")
(* takes in ordered card list (greatest to smallest), returns card list of
straight flush from highest card to lowest, or [] if no straight flush found *)
let rec findStraightFlush cards hold =
  if List.length hold = 5 then List.rev hold else
  match cards with
    | [] -> []
    | h::t ->
      match hold with
        | [] -> findStraightFlush t [h]
        | _ -> if h.num + 1 = (List.hd hold).num && h.suit = (List.hd hold).suit
          then findStraightFlush t (h::hold)
          else findStraightFlush t [h]
(* takes in ordered card list (greatest to smallest), returns card list of
royal flush from highest card to lowest, or [] if no royal flush found *)
let rec findRoyalFlush cards =
  if findStraightFlush cards [] <> [] && (List.hd (findStraightFlush cards [])).num = 14
  then findStraightFlush cards [] else []
(* takes in card list and runs the above functions in sequence starting from
royal flush, then returns hand * card list tuple for each individual player *)
let best_hand seven_cards =
  let sortedNums = getIntList seven_cards [] in
  let sortedCards = sortCards seven_cards sortedNums [] in
  if findRoyalFlush sortedCards != []
    then (Royal_Flush,(findRoyalFlush sortedCards))
  else if findStraightFlush sortedCards [] != []
    then (Straight_Flush,(findStraightFlush sortedCards []))
  else if findFourKind sortedCards [] sortedCards != []
    then (Four_Kind,(findFourKind sortedCards [] sortedCards))
  else if findFullHouse sortedCards [] != []
    then (Full_House,(findFullHouse sortedCards []))
  else if findFlush sortedCards [] [] [] [] != []
    then (Flush,(findFlush sortedCards [] [] [] []))
  else if findStraight sortedCards [] != []
    then (Straight,(findStraight sortedCards []))
  else if findThreeKind sortedCards [] != []
    then (Three_Kind,(findThreeKind sortedCards []))
  else if findTwoPair sortedCards [] [] sortedCards != []
    then (Two_Pair,(findTwoPair sortedCards [] [] sortedCards))
  else if findPair sortedCards [] sortedCards != []
    then (Pair,(findPair sortedCards [] sortedCards))
  else (High_Card,(findHighCard sortedCards))

(* takes in hand * card list tuple returned by best_hand and returns an int list
used later to calcualte which card lists beat out other card lists *)
let hierarchy res =
  match res with
    | High_Card,cards -> List.append [1] (getIntList cards [])
    | Pair,cards -> List.append [2] (getIntList cards [])
    | Two_Pair,cards -> List.append [3] (getIntList cards [])
    | Three_Kind,cards -> List.append [4] (getIntList cards [])
    | Straight,cards -> List.append [5] (getIntList cards [])
    | Flush,cards -> List.append [6] (getIntList cards [])
    | Full_House,cards -> List.append [7] (getIntList cards [])
    | Four_Kind,cards -> List.append [8] (getIntList cards [])
    | Straight_Flush,cards -> List.append [9] (getIntList cards [])
    | Royal_Flush,cards -> List.append [10] (getIntList cards [])

(* [delFirst lst] takes in a ('a * 'b list) list and deletes the
   first element of every 'b list for each element in [lst].
   - requires: [lst] is a ('a * 'b list) list *)
let delFirst winnersList =
  let f tup =
    match tup with
    | a,b -> (a,(List.tl b))
  in
  List.map f winnersList

(* [multWinners lst] takes in a ('a * 'b) list and returns a list of all the
   first element in the tuples.
   - requires: [lst] is a ('a * 'b) list *)
let multWinners winnersList =
  let f tup =
    match tup with
    | a,b -> a
  in
  List.map f winnersList

(* [calc_winner lst] returns a list of the final winners.
   - requires: [lst] is a (player id * int list) list *)
let rec calc_winner specialLst =
  match specialLst with
  | [] -> raise (Failure "No Winner")
  | (a,[])::t -> a::(multWinners t)
  | (a,b)::[] -> [a]
  | (a,b)::t ->
    let maxx = List.hd b in
    let rec getMax lst winners m =
      match lst with
      | [] -> calc_winner (delFirst winners)
      | (c,d)::u ->
        if List.hd d > m then getMax u [(c,d)] (List.hd d)
        else if List.hd d = m then getMax u ((c,d)::winners) m
        else getMax u winners m
    in
    getMax t [(a,b)] maxx

(*[winner state] takes in a state in which the round has ended and returns the
 * player who has won that round. The winner of each round is determined
 * through comparing the [best_hand] of each player, and determining which
 * player has the highest-ranking hand.*)
let winner gs =
  let rec playersLoop players special =
    match players with
    | [] -> calc_winner special
    | h::t ->
      match get_hand h with
      | None -> raise (Failure "Player in queue has no cards: option error")
      | Some (a,b) ->
        playersLoop t ((h,(hierarchy (best_hand (a::b::(public_hand gs)))))::special)
  in
  playersLoop (alive gs) []
(*==========================Calculate_score_above============================= *)

(* [end' st] updates the state after the current round has ended. It should
   calculate who the winner is, and update the players balance accordingly.
   The returned state is the new init state for the next round. *)
let end' st =
  let rec rotate_list new_hd lst =
    if List.hd lst = new_hd
    then lst
    else rotate_list new_hd (List.tl lst @ [List.hd lst])
  in let find_winner st =
       if List.length st.alive = 1
       then st.alive
       else if st.stage = Showdown
       then winner st
       else failwith "[end'] should not have been called: round has not ended"
  in let next_round st winner =
       let rec next_dealer p d alive =
         if (List.length p <= 2) && ((List.hd p).balance = 0)
         then (List.hd p).name
         else if (List.hd p).name = d || (List.hd p).balance = 0
         then next_dealer (List.tl p @ [List.hd p]) d alive
         else (List.hd p).name
       in let clear_hands = List.map (reset_player) st.players
       in let alive' = List.filter (fun p -> p.balance > 0) clear_hands
       in let dealer' = next_dealer (rotate_list (getplayer clear_hands st.smallblind)
                                       clear_hands) st.dealer alive'
       in let order' = rotate_list (getplayer alive' dealer') alive'
       in let init_st =
            {st with alive = List.tl order' @ [List.hd order'];
                     order = order';
                     dealer = dealer';
                     stage = Deal;
                     prev_bets = [];
                     smallblind = if List.length (order') = 2
                       then dealer' else (next order' dealer').name;
                     bigblind = if (List.length (order') = 2)
                       then (next order' dealer').name
                       else (next order' (next order' dealer').name).name;
                     pot = 0; all_in = false; last_raise = 0; last_bet = 0;
                     public_hand = [];
                     message = "New round. Dealer: " ^ dealer';
                     deck = new_deck()}
       in if List.length init_st.alive <= 1
       then {init_st with message = "Game Winner: " ^
                                    (List.nth init_st.alive 0).name ^
                                    ". Please press esc to exit the game. " ^
                                    "Thanks for playing!"}
       else init_st |> deal_to_players |> bet_blinds
  in let winners = find_winner st in
  let money = st.pot / (List.length winners) in
  let update_win st p =
    let p' = {p with balance = p.balance + money} in
    {st with players = update st.players p' [];
             order = update st.order p' [];
             alive = update st.alive p' []}
  in let st_balances = List.fold_left update_win st winners
  in let re_init_players st p =
       let p' = reset_player p in
       {st with players = update st.players p' [];
                order = update st.order p' [];
                alive = update st.alive p' []}
 in let st' = List.fold_left re_init_players st_balances st_balances.players
  in let winners_str = List.fold_left (fun acc p -> acc ^ p.name ^ ", ")
         "" (find_winner st) in
  let alive' = List.filter (fun p -> p.balance > 0) st'.players
  in if List.length (alive') = 1
  then let winner_lst st =
    if List.length st.alive = 1
    then st.alive
    else if st.stage = Showdown
    then winner st
    else failwith "[end'] should not have been called: round has not ended"
    in let winners = winner_lst st in
    let winners_str = List.fold_left (fun acc p -> acc ^ p.name ^ ". ")
        "" winners in
    {st with stage = Empty; message = "Game Winner: " ^ winners_str
                                      ^ " thank you for playing!" ^
                                      " Press esc to exit the game."}
  else next_round st' winners_str

(*[win_state st] should only be called AFTER a round is finished.
  It will return a [player list * bool] tuple, with a list of the winners of
  the finished round, and whether the players' cards should be showed.*)
let win_state st =
  let winner_lst st =
    if List.length st.alive = 1
    then (st.alive, false)
    else if st.stage = Showdown
    then (winner st, true)
    else failwith "[end'] should not have been called: round has not ended"
  in let winners = winner_lst st in
  let winners_str = List.fold_left (fun acc p -> acc ^ p.name ^ ", ")
      "" (fst winners) in
  ({st with stage = Empty; message = "Winner(s): " ^ winners_str
                                    ^ " press enter to continue! "},
   snd winners)

(* helper function for call in do' *)
let call_helper st update_curr_p new_pot all prev =
  let allin = if all = true then all else st.all_in in
  let new_alive = update st.alive update_curr_p [] in
  let new_players = update st.players update_curr_p [] in
  let curr_p = current_player st in
  let new_order = remove st.order curr_p [] in
  if (new_order = []) then (
    (* end of stage *)
    match stage st with
    | Deal -> (
        if (allin = true || st.all_in = true) then (
          (* end of stage with an all-in *)
          {st with order = new_alive;
                   alive = new_alive;
                   players = new_players;
                   pot = new_pot;
                   stage = Showdown;
                   message = curr_p ^ " called.";
                   all_in = true;} |> begin_deal |> turn_deal |> turn_deal

        ) else (
          let deal_st = begin_deal st in
          let new_curr = name (List.hd (new_alive)) in
          let new_msg = curr_p ^ " called. The flop is shown. Next player: "
                        ^ new_curr in
          {deal_st with order = new_alive;
                        alive = new_alive;
                        players = new_players;
                        pot = new_pot;
                        currentplayer = new_curr;
                        stage = Flop;
                        message = new_msg;
                        all_in = allin;
                        last_bet = 0;
                        last_raise = 0;
                        prev_bets = [];
          }
        )
      )
    | Flop -> (
        if (allin = true || st.all_in = true) then (
          (* end of stage with an all-in *)
          {st with order = new_alive;
                   alive = new_alive;
                   players = new_players;
                   pot = new_pot;
                   stage = Showdown;
                   message = curr_p ^ " called.";
                   all_in = true;} |> turn_deal |> turn_deal

        ) else
        let deal_st = turn_deal st in
        let new_curr = name (List.hd (new_alive)) in
        let new_msg = curr_p ^ " called. The turn is shown. Next player: "
                      ^ new_curr in
        {deal_st with order = new_alive;
                      alive = new_alive;
                      players = new_players;
                      pot = new_pot;
                      currentplayer = new_curr;
                      stage = Turn;
                      message = new_msg;
                      all_in = allin;
                      last_bet = 0;
                      last_raise = 0;
                      prev_bets = [];
        }
      )
    | Turn -> (
        if (allin = true || st.all_in = true) then (
          (* end of stage with an all-in *)
          {st with order = new_alive;
                   alive = new_alive;
                   players = new_players;
                   pot = new_pot;
                   stage = Showdown;
                   message = curr_p ^ " called.";
                   all_in = true;} |> turn_deal

        ) else
        let deal_st = turn_deal st in
        let new_curr = name (List.hd (new_alive)) in
        let new_msg = curr_p ^ " called. The River is shown. Next player: "
                      ^ new_curr in
        {deal_st with order = new_alive;
                      alive = new_alive;
                      players = new_players;
                      pot = new_pot;
                      currentplayer = new_curr;
                      stage = River;
                      message = new_msg;
                      all_in = allin;
                      last_bet = 0;
                      last_raise = 0;
                      prev_bets = [];
        }
      )
    | River -> {st with order = new_alive;
                             alive = new_alive;
                             players = new_players;
                             pot = new_pot;
                             stage = Showdown;
                             message = curr_p ^ " called.";
                             all_in = allin;
                    }
    | Showdown -> raise (Failure "Call command in showdown.")
    | Empty -> failwith "Empty"
  ) else (
    (* not end of stage *)
    let new_curr = name (next new_alive curr_p) in
    let new_msg = curr_p ^ " called. Next player: " ^ new_curr in
    {st with order = new_order;
             alive = new_alive;
             players = new_players;
             pot = new_pot;
             currentplayer = new_curr;
             message = new_msg;
             all_in = allin;
             prev_bets = prev;
    }
  )


(* [do' c] is the new state after the command [c]. Command [c] should give
   information on whether the current player call, raise, fold, or checks.
   If the command is not permitted (ex: betting below the min bet), the state
   does not update any of its fields except for show_message, which should show
   an error message that prompts the same user for a valid command. *)
let rec do' c st =
  match c with
  | Raise i -> (
      let curr_p = current_player st in
      let curr_player = getplayer st.players curr_p in
      let curr_player = {curr_player with denom = curr_player.denom + 1;} in
      let bet = st.last_bet + i - (get_prevbet st.prev_bets curr_p) in
      let new_prev = new_prevbets st.prev_bets curr_p bet [] in
      if (i = 0) then (
        {st with message = "Cannot raise $0. Please try again."}
      ) else if (st.all_in = true) then (
        (* a player already went all-in. cannot raise. *)
        {st with
         message = "A player has already gone all-in in this stage. Cannot raise during this stage. Please try again."}
      ) else if (bet > balance curr_player) then (
        (* raise too much *)
        {st with
         message = "Player " ^ curr_p ^
                   " does not have enough balance to raise $"
                   ^ Pervasives.string_of_int i ;
        }
      ) else if (bet = balance curr_player) then (
        (* all - in *)
        let new_balance = 0 in
        let update_curr_p = set_balance curr_player new_balance in
        let update_curr_p = set_all_in update_curr_p true in
        let new_pot = st.pot + balance curr_player in
        let new_players = update st.players update_curr_p [] in
        let new_alive = update st.alive update_curr_p [] in
        let rec get_append alive id acc =
          match alive with
          | [] -> acc
          | h::t ->
            if (name h = id) then List.append t (List.rev acc)
            else if (all_in h = false) then get_append t id (h::acc)
                else get_append t id acc in
        let new_order = get_append new_alive curr_p [] in
        if (new_order = []) then (
          (* end of stage *)
          raise (Failure "Raise with new order empty for all in")
        ) else (
          (* not end of stage *)
          let next_curr = name (List.hd new_order) in
          let new_msg = curr_p ^ " went all-in with $" ^
                        Pervasives.string_of_int bet
                        ^ ". Next player: " ^ next_curr in
          let new_raise = if (i > st.last_raise) then i else st.last_raise in
          let new_bet = st.last_bet + i in
          {st with order = new_order;
                   alive = new_alive;
                   players = new_players;
                   pot = new_pot;
                   currentplayer = next_curr;
                   message = new_msg;
                   last_raise = new_raise;
                   last_bet = new_bet;
                   all_in = true;
                   prev_bets = new_prev;
          }
        )
      ) else if (i < st.last_raise) then (
        (* smaller than min raise *)
        {st with
         message = "Minimum raise is $" ^ Pervasives.string_of_int st.last_raise
                   ^ ". Please try again.";
        }
      ) else (
        (* balance enough *)
        let new_balance = balance curr_player - bet in
        let update_curr_p = set_balance curr_player new_balance in
        let new_players = update st.players update_curr_p [] in
        let new_alive = update st.alive update_curr_p [] in
        let new_pot = st.pot + bet in
        let rec get_append alive id acc =
          match alive with
          | [] -> acc
          | h::t ->
            if (name h = id) then List.append t (List.rev acc)
            else if (all_in h = false) then get_append t id (h::acc)
                else get_append t id acc in
        let new_order = get_append new_alive curr_p [] in
        if (new_order = []) then (
          (* end of stage *)
          raise (Failure "Raise with new order empty.")
        ) else (
          (* not end of stage *)
          let next_curr = name (List.hd new_order) in
          let new_msg = curr_p ^ " raised $" ^ Pervasives.string_of_int i
                        ^ ". Next player: " ^ next_curr in
          {st with order = new_order;
                   alive = new_alive;
                   players = new_players;
                   pot = new_pot;
                   currentplayer = next_curr;
                   message = new_msg;
                   last_raise = i;
                   last_bet = i + st.last_bet;
                   prev_bets = new_prev;
          }
        )
      )
    )
  | Call -> (
      let curr_p = current_player st in
      let curr_player = getplayer st.players curr_p in
      let bet = st.last_bet - (get_prevbet st.prev_bets curr_p) in
      let new_prev = new_prevbets st.prev_bets curr_p bet [] in
      let new_balance = balance curr_player - bet in
      let curr_player = {curr_player with c_numer = curr_player.c_numer + 1;
                                          denom = curr_player.denom + 1;} in
      if (new_balance <= 0) then (
        (* not enough balance to call. all-in *)
        let new_balance = 0 in
        let update_curr_p = set_balance curr_player new_balance in
        let update_curr_p = set_all_in update_curr_p true in
        let new_pot = st.pot + balance curr_player in
        call_helper st update_curr_p new_pot true new_prev
      ) else (
        (* balance enough to call. *)
        let update_curr_p = set_balance curr_player new_balance in
        let new_pot = st.pot + bet in
        call_helper st update_curr_p new_pot false new_prev
      )
    )
  | Fold -> (
      let curr_p = current_player st in
      let new_order = remove st.order curr_p [] in
      let new_alive = remove st.alive curr_p [] in
      let old_p = getplayer st.players curr_p in
      let new_p = {old_p with f_numer = old_p.f_numer + 1;
                              denom = old_p.denom + 1;} in
      let new_players = update st.players new_p [] in
      if (List.length new_alive = 1) then (
        (* only one player left. *)
        {st with order = new_order;
                      alive = new_alive;
                      players = new_players;
                      stage = Showdown;
                      message = curr_p ^ " folded.";
             }
      ) else if (new_order = []) then (
        (* end of stage *)
        match stage st with
        | Deal -> (
            if (st.all_in = true) then (
              (* end of stage with an all-in *)
              {st with order = new_alive;
                       alive = new_alive;
                       players = new_players;
                       stage = Showdown;
                       message = curr_p ^ " folded.";}
              |> begin_deal |> turn_deal |> turn_deal

            ) else
            let deal_st = begin_deal st in
            let new_curr = name (List.hd new_alive) in
            let new_msg = curr_p ^ " folded. The flop is shown. Next player: "
                          ^ new_curr in
            {deal_st with order = new_alive;
                          alive = new_alive;
                          players = new_players;
                          currentplayer = new_curr;
                          stage = Flop;
                          message = new_msg;
                          last_bet = 0;
                          last_raise = 0;
                          prev_bets = [];}
          )
        | Flop -> (
            if (st.all_in = true) then (
              (* end of stage with an all-in *)
              {st with order = new_alive;
                       alive = new_alive;
                       players = new_players;
                       stage = Showdown;
                       message = curr_p ^ " folded.";}
              |> turn_deal |> turn_deal
            ) else
            let deal_st = turn_deal st in
            let new_curr = name (List.hd new_alive) in
            let new_msg = curr_p ^ " folded. The turn is shown. Next player: "
                          ^ new_curr in
            {deal_st with order = new_alive;
                          alive = new_alive;
                          players = new_players;
                          currentplayer = new_curr;
                          stage = Turn;
                          message = new_msg;
                          last_bet = 0;
                          last_raise = 0;
                          prev_bets = [];
            }
          )
        | Turn -> (
            if (st.all_in = true) then (
              (* end of stage with an all-in *)
              {st with order = new_alive;
                       alive = new_alive;
                       players = new_players;
                       stage = Showdown;
                       message = curr_p ^ " folded.";}
              |> turn_deal
            ) else
            let deal_st = turn_deal st in
            let new_curr = name (List.hd new_alive) in
            let new_msg = curr_p ^ " folded. The river is shown. Next player: "
                          ^ new_curr in
            {deal_st with order = new_alive;
                          alive = new_alive;
                          players = new_players;
                          currentplayer = new_curr;
                          stage = River;
                          message = new_msg;
                          last_bet = 0;
                          last_raise = 0;
                          prev_bets = [];
            }
          )
        | River -> (
            (* end of this whole round *)
            {st with order = new_alive;
                     alive = new_alive;
                     players = new_players;
                     stage = Showdown;
                     message = curr_p ^ " folded.";
                 }
          )
        | Showdown -> raise (Failure "Fold command in showdown.")
        | Empty -> failwith "empty"
      ) else (
        (* not end of stage *)
        let next_curr = name (List.hd new_order) in
        let new_msg = curr_p ^ " folded. Next player: " ^ next_curr in
        {st with order = new_order;
                 alive = new_alive;
                 players = new_players;
                 currentplayer = next_curr;
                 message = new_msg;
        }
      )

    )
  | Show -> {st with message = st.currentplayer ^ "'s hand: " ^
                               string_of_hand (getplayer st.order st.currentplayer)}
  | None -> {st with message = "Not a valid command"}
  | Exit -> {st with message = "Exiting..."}
  | Manual -> {st with message = "How to Use this REPL: \n
raise <int>                          -- raises the current player's bet by <int>, if permitted by game rules.\n
call                                 -- current player elects to follow the current bet.\n
fold                                 -- current player forfeits the round.\n
show                                 -- shows the current player's hand, given a round is currently taking place.\n
status                               -- shows the stats of the current round and overall game.\n
manual                               -- prints out instructions on using this REPL (the instructions written here)\n
exit                                 -- exits the game.\n"}
  | Status -> {st with message = "Current game status:\n
Players currently in game: " ^ string_of_players st ^ "\n
Queue: " ^ string_of_order st ^ "\n
Dealer: " ^ st.dealer ^ "\n
Small Blind: " ^ st.smallblind ^ "\n
Big Blind: " ^ st.bigblind ^ "\n
Money in pool: " ^ string_of_int st.pot ^"\n
Game stage: " ^ string_of_stage st ^ "\n
Last raise: " ^ string_of_int st.last_raise ^"\n
Last bet: " ^ string_of_int st.last_bet ^ "\n
Cards remaining in deck: " ^ string_of_int (List.length st.deck) ^ "\n
Previous bets: " ^ string_of_prevbets st ^ "\n
Current player balances: " ^ string_of_balances st ^ "\n
Public hand: " ^ string_of_public_hand st ^ "\n
Current player: " ^ st.currentplayer ^ "\n"
}
