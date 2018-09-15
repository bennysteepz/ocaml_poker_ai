open Player
open Gamestate
open Command


(* indicates the type of move that is appropriate.
   Red stands for 3-betting, Yellow stands for calling a raise,
   Green stands for openning a raise but not calling one, and
   White stands for folding. *)
type move =
  | Red
  | Yellow
  | Green
  | White

(* List of all the possible suits. *)
let suitlst = [Spade;Heart;Diamond;Club;]


(* returns the hutchison for only one card [c] *)
let hutchison c =
  if c.num = 14 then 16
  else if c.num = 13 then 14
  else if c.num = 12 then 13
  else if c.num = 11 then 12
  else if c.num = 10 then 11
  else c.num

(* [hutchison_pair c1 c2] calculates the hutchison score for the hand c1,c2 *)
let hutchison_pair c1 c2 =
  let score = (hutchison c1) + (hutchison c2) in
  if c1.num = c2.num then score + 10
  else if c1.suit = c2.suit then (
    (* same suit + 4 *)
    if (Pervasives.abs (c1.num - c2.num) = 1) then score + 7
    else if (Pervasives.abs (c1.num - c2.num) = 2) then score + 6
    else if (Pervasives.abs (c1.num - c2.num) = 3) then score + 5
    else score + 4
  ) else (
    (* not same suit *)
    if (Pervasives.abs (c1.num - c2.num) = 1) then score + 3
    else if (Pervasives.abs (c1.num - c2.num) = 2) then score + 2
    else if (Pervasives.abs (c1.num - c2.num) = 3) then score + 1
    else score
  )

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
    else add_card {card with num = card.num + 2} (card :: lst)
  in add_card {suit = Diamond; num = 2} []

(* [remove_deck deck lst] returns a deck of cards with the cards in [lst]
   removed from [deck].
   - requires: [lst] & [deck] are card lists *)
let remove_deck deck lst =
  let rec loop d l acc =
    match d with
    | [] -> acc
    | h::t ->
      let rec check cardlst ac =
        match cardlst with
        | [] -> loop t l (h::acc)
        | a::b ->
          if (h.num = a.num && h.suit = a.suit) then loop t (a::ac) acc
          else check b (a::ac) in
      check l [] in
  loop deck lst []

(* [random_card d] returns a card by random from card list [d] *)
let random_card d =
  let () = Random.init (int_of_float (Unix.time())) in
  let i = Random.int (List.length d) in List.nth d i


(* [get_postition st p] returns 0 if the player [p] is the dealer,
   returns 1 if the player [p] is the small blind,
   Returns 2 if the player [p] is the big blind,
   returns 3 if the player [p] is the player after big blind.*)
let get_position st p =
  if (name p = name (dealer st)) then 0
  else if (name p = name (small_blind st)) then 1
  else if (name p = name (big_blind st)) then 2
  else 3

(* [to_percentage n1 n2] is the percentage for n1 / n2.
   - returns: an int between 0 and 100
   - requires: n1 <= n2 and n1, n2 are both ints
   - example: if n1/n2 = 0.5, then the function will return 50 *)
let to_percentage n1 n2 =
  Pervasives.int_of_float ((Pervasives.float n1) /. (Pervasives.float n2) *. 100.0)

(* [outs lst st] calculates the number of outs given a list of cards [lst]
   and current state [st].
   - requires: [lst] is a card list that contains the community cards and
               the current player's hand.*)
let outs lst st =
  let deck = deck st in
  let rec loop_deck d acc =
    match d with
    | [] -> acc
    | h::t ->
      match best_hand (h::lst) with
      | Royal_Flush, lst -> loop_deck t (h::acc)
      | Straight_Flush, lst -> loop_deck t (h::acc)
      | Four_Kind, lst -> loop_deck t (h::acc)
      | Full_House, lst -> loop_deck t (h::acc)
      | Flush, lst -> loop_deck t (h::acc)
      | Straight, lst -> loop_deck t (h::acc)
      | Three_Kind, lst -> loop_deck t acc
      | Two_Pair, lst -> loop_deck t acc
      | Pair, lst -> loop_deck t acc
      | High_Card, lst -> loop_deck t acc in
  loop_deck deck []

(* range for action against 3-betting in heads up poker.
   - requires: [c1], [c2] are cards with c1.num >= c2.num
   - resource: https://upswingpoker.com/how-to-3-bet-in-heads-up-no-limit/
   - returns: 0 for 3-betting, 1 for calling, and 2 for folding. *)
let three_bet_hdup c1 c2 =
  if (c1.num = c2.num && c1.num >= 9) then 0 (* 3 bet for AA - 99 *)
  else if (c1.num = c2.num) then 1 (* call for pairs 88 - *)
  else if (c1.suit = c2.suit && c2.num >= 10) then 0 (* 3 bet for AKs - JTs *)
  else if (c1.suit = c2.suit && c1.num - c2.num <= 1 && c2.num >= 7) then 0 (* 87s - T9s *)
  else if (c1.suit = c2.suit && c2.num = 2 && (c1.num = 6 || c1.num = 4)) then 0 (* 62s, 42s *)
  else if (c1.suit = c2.suit) then Random.int 2
  else if (c1.num + c2.num >= 25) then 0 (* AK, AQ, AJ, KQo *)
  else if (c2.num = 5 && c1.num <= 9 && c1.num >= 7) then 0 (* 95, 85, 75o *)
  else if (c2.num = 4 && (c1.num = 10 || c1.num = 6 || c1.num = 5)) then 0 (* T4, 64, 54o *)
  else if (c1.num + c2.num >= 14) then 1
  else 2

(* [hierarchy res] takes in an output form best_hand in gamestate,
   and turns the hand to a list of ints according to the strength of the hand
   that are used for comparison later.*)
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

(* [calc_win lst] returns a list with all the winners' ids.
   - requires: [lst] is (player * (hand * card list)) list *)
let calc_win lst =
  let rec loop l acc =
    match l with
    | [] -> calc_winner acc
    | (p, bh)::t ->
      let hier = hierarchy bh in
      loop t ((p,hier)::acc) in
  loop lst []


(* Takes in 2 cards [c1] and [c2], and returns the correct move type
   if the player is a dealer that is playing in the loose range.
   - requires: [c1].num >= [c2].num *)
let dealer_range_loose c1 c2 =
  if c1.num = 9 && c2.num = 7 && c1.suit = c2.suit then Red
  else if c1.num = 7 && c2.num = 5 && c1.suit = c2.suit then Red
  else if c1.num = 14 && c2.num >= 12 then Red
  else if c1.num = 14 && c2.num >= 10 && c1.suit = c2.suit then Red
  else if c1.num = 14 && c1.suit = c2.suit then Yellow
  else if c1.num = 14 && c2.num >= 10 then Yellow
  else if c1.num = 14 && c2.num >= 7 then Green
  else if c1.num = c2.num && c1.num >= 9 then Red
  else if c1.num = c2.num then Yellow
  else if c1.num - c2.num <= 1 && c1.suit = c2.suit && c2.num >= 10 then Red
  else if c1.num = 13 && c2.num = 11 && c1.suit = c2.suit then Red
  else if c1.num + c2.num >= 24 then Yellow
  else if c1.suit = c2.suit && c1.num - c2.num = 1 && c1.num >= 4 then Yellow
  else if c1.suit = c2.suit && c1.num - c2.num = 2 && c1.num >= 5 then Yellow
  else if c1.suit = c2.suit && c1.num - c2.num = 3 && c1.num >= 10 then Yellow
  else if c1.suit = c2.suit && c1.num = 13 && c2.num = 9 then Yellow
  else if c1.suit = c2.suit && c1.num = 13 then Green
  else if c1.suit = c2.suit && c1.num = 12 && c2.num >= 5 then Green
  else if c1.suit = c2.suit && c1.num = 11 && c2.num = 7 then Green
  else if c1.suit = c2.suit && c1.num = 9 && c2.num = 6 then Green
  else if c1.num + c2.num >= 22 then Green
  else if c1.num = 14 && c2.num = 7 then Green
  else if c1.num = 11 && c2.num = 10 then Green
  else White


(* Takes in 2 cards [c1] and [c2], and returns the correct move type
   if the player is the cut off that is playing in the loose range.
   - requires: [c1].num >= [c2].num *)
let cutoff_range_loose c1 c2 =
  if c1.num + c2.num >= 27 then Red
  else if c1.num = c2.num && c1.num >= 11 then Red
  else if c1.suit = c2.suit && c1.num = 10 && c2.num = 9 then Red
  else if c1.suit = c2.suit && c1.num = 8 && c2.num = 7 then Red
  else if c1.suit = c2.suit && c1.num = 5 && c2.num = 4 then Red
  else if c1.num = c2.num then Yellow
  else if c1.suit = c2.suit && c1.num - c2.num <= 1 && c1.num >= 7 then Yellow
  else if c1.suit = c2.suit && c1.num = 14 then Yellow
  else if c1.suit = c2.suit && c1.num >= 12 && c2.num >= 10 then Yellow
  else if c1.suit = c2.suit && c1.num - c2.num <= 2 && c2.num >= 3 then Green
  else if c1.suit = c2.suit && c1.num = 13 && c2.num >= 7 then Green
  else if c1.num = 12 && c2.num = 9 then Green
  else if c1.num = 14 && c2.num = 12 then Yellow
  else if c1.num + c2.num >= 24 then Green
  else White

(* Takes in 2 cards [c1] and [c2], and returns the correct move type
   if the player is a blind that is playing in the loose range,
   facing a raise.
   - requires: [c1].num >= [c2].num *)
let blind_vs_raise_loose c1 c2 =
  if c1.num + c2.num >= 27 then Red
  else if c1.num = c2.num && c1.num >= 12 then Red
  else if c1.num = 10 && c2.num = 9 && c1.suit = c2.suit then Red
  else if c1.num = 8 && c2.num = 7 && c1.suit = c2.suit then Red
  else if c1.num = 14 then Yellow
  else if c1.num = c2.num then Yellow
  else if c1.suit = c2.suit && c1.num - c2.num <= 1 && c2.num >= 6 then Yellow
  else if c1.suit = c2.suit && c2.num >= 10 then Yellow
  else if c1.num = 14 && c2.num = 12 then Yellow
  else White

  (* Takes in 2 cards [c1] and [c2], and returns the correct move type
     if the player is a blind that is playing in the loose range,
     facing a limper.
     - requires: [c1].num >= [c2].num *)
  let blind_vs_limper_loose c1 c2 =
    if c1.num = c2.num && c1.num >= 9 then Green
    else if c1.num = 14 && c1.suit = c2.suit then Green
    else if c1.suit = c2.suit && c1.num = 13 && (c2.num >= 11 || c2.num = 8) then Green
    else if c1.suit = c2.suit && c1.num = 7 && c2.num = 6 then Green
    else if c1.num = 14 && c2.num = 12 then Green
    else White

(* [montecarlo public c1 c2] returns a boolean that indicates the ai's chances
   of winning in a heads up game, if given the cards [c1], [c2], and
   community cards [public].
   - returns: true if the chance is > 50%, false if the chance is < 50%
     after running the simulation multiple times.
   - requires: [c1], [c2] are cards, and [pulbic] is a card list. *)
let montecarlo public c1 c2 =
  let deck = remove_deck (new_deck()) (c1::c2::public) in
  let rec checkAI lst =
    match lst with
    | [] -> false
    | a::b -> if a = "ai" then true else checkAI b in
  let rec simulation n wins =
    if n = 0 then (if 2 * wins >= 1000 then true else false)
    else (
      let opp_card1 = random_card deck in
      let d1 = remove_deck deck [opp_card1] in
      let opp_card2 = random_card d1 in
      let d2 = remove_deck d1 [opp_card2] in
      let community =
        if (List.length public >= 5) then public
        else if (List.length public = 4) then (
          (random_card d2)::public
        ) else (
          let c4 = random_card d2 in
          let d3 = remove_deck d2 [c4] in
          let c5 = random_card d3 in
          c4::c5::public
        ) in
      let op_bh = best_hand (opp_card1::opp_card1::community) in
      let ai_bh = best_hand (c1::c2::community) in
      let winnerlst = calc_win (("op", op_bh)::("ai", ai_bh)::[]) in
      if checkAI winnerlst = true then simulation (n-1) (wins + 1)
      else simulation (n-1) wins
    ) in
  simulation 1000 0

(* helper function for adding pairs.
   - requires: [num] is a list of number for the pairs to be added. *)
let add_pair num =
  let rec loop_num n lst =
    match n with
    | [] -> lst
    | h::t ->
      let rec loop_suit s acc =
        match s with
        | [] -> loop_num t acc
        | a::b ->
          let rec inloop suit ac =
            match suit with
            | [] -> loop_suit b ac
            | x::y ->
              if (x <> a) then
                inloop y (({num=h;suit=a;},{num=h;suit=x;})::ac)
              else inloop y ac in
          inloop suitlst acc in
      loop_suit suitlst lst in
  loop_num num []

(* helper function for adding connected and suited hands, with numbers in
   list [num] as the higher number, and the number - diff as the lower
   number. *)
let add_consuited num diff =
  let rec loopnum n lst =
    match n with
    | [] -> lst
    | h::t ->
      let rec loopsuit suit acc =
        match suit with
        | [] -> loopnum t acc
        | a::b -> loopnum t (({num=h;suit=a;},{num=h-diff;suit=a;})::acc) in
      loopsuit suitlst lst in
  loopnum num []

(* helper function for adding offsuit hands.
   - requires: numlst must be a (int * int) list *)
let add_off numlst =
  let rec loopnumpair n lst =
    match n with
    | [] -> lst
    | (n1,n2)::t ->
      let rec suit1 s1 acc =
        match s1 with
        | [] -> loopnumpair t acc
        | h::t ->
          let rec suit2 s2 ac =
            match s2 with
            | [] -> suit1 t ac
            | a::b ->
              if (a<>h) then
                suit2 b (({num=n1;suit=h;},{num=n2;suit=a;})::ac)
              else suit2 b ac in
          suit2 suitlst acc in
      suit1 suitlst lst in
  loopnumpair numlst []

(* [u4] is a list of hands that are in the top tier range. *)
let u4 =
  let pairs = add_pair [14;13;12;11;] in
  let rec add_ak s1 lst =
    match s1 with
    | [] -> lst
    | h::t ->
      let rec loopksuit s2 acc =
        match s2 with
        | [] -> add_ak t acc
        | a::b -> loopksuit b (({num=14;suit=h;},{num=13;suit=a;})::acc) in
      loopksuit suitlst lst in
  let rec addkq s lst =
    match s with
    | [] -> lst
    | h::t -> addkq t (({num=13;suit=h;},{num=12;suit=h;})::lst) in
  List.append pairs (List.append (add_ak suitlst []) (addkq suitlst []))

(* [u3] is a list of hands that are in the 2nd tier range. *)
let u3 =
  let pairs = add_pair [10;9;] in
  let conn = add_consuited [13;12;11;10;9;] 1 in
  let kjs = add_consuited [13] 2 in
  let rec addajat s1 lst =
    match s1 with
    | [] -> lst
    | h::t ->
      let rec loopsuit s2 acc =
        match s2 with
        | [] -> addajat t acc
        | a::b ->
          let aj = {num=14;suit=h;},{num=11;suit=a;} in
          let at = {num=14;suit=h;},{num=10;suit=a;} in
          loopsuit b (aj::at::acc) in
      loopsuit suitlst lst in
  let rec addaqo s1 lst =
    match s1 with
    | [] -> lst
    | h::t ->
      let rec loopsuit s2 acc =
        match s2 with
        | [] -> addaqo t acc
        | a::b ->
          if (h<>a) then loopsuit b (({num=14;suit=h;},{num=12;suit=a;})::acc)
          else loopsuit b acc in
      loopsuit suitlst lst in
  let rec adda9s s lst =
    match s with
    | [] -> lst
    | h::t -> adda9s t (({num=14;suit=h;},{num=9;suit=h;})::lst) in
  List.flatten [pairs;conn;kjs;(addajat suitlst []);
                (addaqo suitlst []);(adda9s suitlst []);]


(* u2 is a list of hands that are in the middle tier range. *)
let u2 =
  let pairs = add_pair [8;7;6;5;4;3;2;] in
  let conns = add_consuited [8;7;6;5;] 1 in
  let conn2s = add_consuited [12;11;] 2 in
  let conn3s = add_consuited [13;12;] 3 in
  let rec add_k9to7 s lst =
    match s with
    | [] -> lst
    | h::t ->
      let nine = {num=13;suit=h;},{num=9;suit=h;} in
      let eight = {num=13;suit=h;},{num=8;suit=h;} in
      let seven = {num=13;suit=h;},{num=7;suit=h;} in
      add_k9to7 t (nine::eight::seven::lst) in
  let rec addas n lst =
    if (n < 2) then lst
    else
      let rec loopsuit s acc =
        match s with
        | [] -> addas (n-1) acc
        | h::t -> loopsuit t (({num=14;suit=h;},{num=n;suit=h;})::acc) in
      loopsuit suitlst lst in
  let offs = add_off [(13,12);(13,11);(12,11);(14,9);] in
  List.flatten [pairs;conns;conn2s;conn3s;(add_k9to7 suitlst []);
                (addas 8 []); offs;]

(* [u1] is a list of hands that are in the lower tier range. *)
let u1 =
  let offs = add_off [(13,10);(12,10);(11,10);(10,9);(9,8);(13,9);(12,9);
                      (14,8);(14,7);] in
  let conn2s = add_consuited [10;9;8;7;6;5;] 2 in
  let conn = add_consuited [4] 1 in
  let conn3s = add_consuited [11;10;9;] 3 in
  let conn4s = add_consuited [12;11;] 4 in
  let rec add_q7to5 s lst =
    match s with
    | [] -> lst
    | h::t ->
      let seven = {num=12;suit=h;},{num=7;suit=h;} in
      let six = {num=12;suit=h;},{num=6;suit=h;} in
      let five = {num=12;suit=h;},{num=5;suit=h;} in
      add_q7to5 t (seven::six::five::lst) in
  let rec addks n lst =
    if (n < 2) then lst
    else
      let rec loopsuit s acc =
        match s with
        | [] -> addks (n-1) acc
        | h::t -> loopsuit t (({num=13;suit=h;},{num=n;suit=h;})::acc) in
      loopsuit suitlst lst in
  List.flatten [offs;conn2s;conn;conn3s;conn4s;(add_q7to5 suitlst []);
                (addks 6 []);]

(* helper function that removes the hands in [handlst] that
   contains cards in [nolst].
   - requires: [handlst] is a (card * card) list,
               [nolst] is a card list *)
let remove_hand handlst nolst =
  let rec loopno no lst =
    match no with
    | [] -> lst
    | h::t ->
      let rec loopall hand acc =
        match hand with
        | [] -> loopno t acc
        | (c1,c2)::b ->
          if (h.num = c1.num && h.suit = c1.suit) then
            loopall b acc
          else if (h.num = c2.num && h.suit = c2.suit) then
            loopall b acc
          else loopall b ((c1,c2)::acc) in
      loopall lst [] in
  loopno nolst handlst


(* returns a random hand (tuple of 2 cards) given the cards not in [comm]
   and fold probability [f] and call probability [c]. *)
let random_hand comm f c =
  let r = 1.0 -. f -. c in
  let range =
    if r <= 0.03 then 4
    else if r <= 0.1 then 3
    else if r <= 0.21 then 2
    else if r <= 0.35 then 1
    else 0 in
  if range = 4 then (
    let u4' = remove_hand u4 comm in
    random_card u4'
  ) else if range = 3 then (
    let u4' = remove_hand u4 comm in
    let u3' = remove_hand u3 comm in
    random_card (List.append u4' u3')
  ) else if range = 2 then (
    let u4' = remove_hand u4 comm in
    let u3' = remove_hand u3 comm in
    let u2' = remove_hand u2 comm in
    random_card (List.append u4' (List.append u3' u2'))
  ) else if range = 1 then (
    let u4' = remove_hand u4 comm in
    let u3' = remove_hand u3 comm in
    let u2' = remove_hand u2 comm in
    let u1' = remove_hand u1 comm in
    random_card (List.append u4' (List.append u3' (List.append u2' u1')))
  ) else (
    let deck = remove_deck (new_deck()) comm in
    let c1 = random_card deck in
    let deck2 = remove_deck deck [c1] in
    let c2 = random_card deck2 in
    (c1,c2)
  )

(* helper function for sorting a list of cards according to number *)
let sort_card_lst lst =
  let f c1 c2 = c1.num - c2.num in
  List.sort f lst

(* helper function for finding a nut card that is in [deck], given
   the [public] cards on the community table. *)
let findnut public deck =
  let checknewold lst =
    match lst with
    | [] -> raise (Failure "no winner in findnut")
    | a::[] -> if (a = "new") then true else false
    | _ -> false in
  let rec loopdeck d best_h c =
    match d with
    | [] -> c
    | h::t ->
      let curr_h = best_hand (h::public) in
      let winlst = calc_win (("old", best_h)::("new", curr_h)::[]) in
      if (checknewold winlst) then loopdeck t curr_h h
      else loopdeck t best_h c in
loopdeck (List.tl deck) (best_hand ((List.hd deck)::public)) (List.hd deck)


(* [assign_hole deck public comm p] returns a tuple of 2 cards that are
   the hole cards for player [p]. This is calculated using the fold/call
   probability of player [p].
   - requires: [deck] is a card list containing all the possible cards
               that have not been used.
               [public] is the list of cards on the community table.
               [comm] is the list of cards that have already been used,
               either assigned to another player or on the table. In other
               words, [deck] + [comm] = the whole deck of cards. *)
let assign_hole deck public comm p =
  let () = Random.init (int_of_float (Unix.time())) in
  let f_prob = fold_prob p in
  let c_prob = call_prob p in
  let ptop =
    if f_prob < (1.0 /. 3.0) then 3.0 *. f_prob *. f_prob
    else (1.0 /. 3.0) in
  let pnut =
    if f_prob < (1.0 /. 3.0) then 0.0
    else if f_prob >= (2.0 /. 3.0) then f_prob -. (1.0 /. 3.0)
    else (1.0 /. 3.0) *. (3.0 *. f_prob -. 1.0) *. (3.0 *. f_prob -. 1.0) in
  let pother = 1.0 -. ptop -. pnut in
  let randf = Random.float 1.0 in
  let generateposs top =
    let rec loopsuit s lst =
      match s with
      | [] -> lst
      | h::t ->
        if h = top.suit then loopsuit t lst
        else loopsuit t ({num=top.num;suit=h;}::lst) in
    loopsuit suitlst [] in
  let checkcomm nolst poss =
    let rec loopno no po =
      match no with
      | [] -> po
      | h::t ->
        let rec looppo p acc =
          match p with
          | [] -> loopno t acc
          | a::b ->
            if (h.num = a.num && h.suit = a.suit) then looppo b acc
            else looppo b (a::acc) in
        looppo po [] in
    loopno nolst poss in
  if randf <= pother then random_hand comm f_prob c_prob
  else if randf <= pother +. ptop then (
    let topcd = List.hd (sort_card_lst public) in
    let topposs = checkcomm comm (generateposs topcd) in
    if topposs = [] then random_hand comm f_prob c_prob
    else
      let c1 = random_card topposs in
      let c2 = random_card (remove_deck deck (c1::comm)) in
      (c1,c2)
  ) else (
    let c1 = findnut public deck in
    let c2 = random_card (remove_deck deck (c1::comm)) in
    (c1,c2)
  )


(* helper function that runs monte carlo simulation for multiple players.
   Takes in a state [st] and the AI's 2 cards [c1] and [c2].
   - returns: 0 for fold, 1 for call, and 2 for raise *)
let multimonte st c1 c2 =
  let public = public_hand st in
  let alive = alive st in
  let deck = remove_deck (new_deck()) (c1::c2::public) in
  let calc_prev_investment =
    let rec loop_prev prevlst acc prev =
      match prevlst with
      | [] -> ((pot st) - acc) / (List.length alive) + prev
      | (n,p)::t ->
        if (n = current_player st) then loop_prev t (acc + p) p
        else loop_prev t (acc + p) prev in
    loop_prev (prev_bets st) 0 0 in
  let rec checkAI winnerlst =
    match winnerlst with
    | [] -> false
    | a::b -> if a = "Hard AI" then true else checkAI b in
  let rec calc_money winnerlst pot =
    let len = List.length winnerlst in
    if (len >= 1) then pot / len
    else raise (Failure "calc_money winnerlst length < 0") in
  let rec simulation n exp r =
    if n = 0 then exp
    else (
      let rec loop_alive ali acc d c =
        match ali with
        | [] -> acc,d
        | h::t ->
          let (h1,h2) = assign_hole d public c h in
          loop_alive t ((h, h1, h2)::acc) (remove_deck d [h1;h2]) (h1::h2::c) in
      let (holelst,deck) = loop_alive alive [] deck (c1::c2::public) in
      let community =
        if (List.length public >= 5) then public
        else if (List.length public = 4) then (random_card deck)::public
        else (
          let c4 = random_card deck in
          let d4 = remove_deck deck [c4] in
          let c5 = random_card d4 in
          c4::c5::public
        ) in
      let rec gen_winnerlst holelst acc =
        match holelst with
        | [] -> calc_win acc
        | (p,h1,h2)::t ->
          let () = Random.init (int_of_float (Unix.time())) in
          let randf = Random.float 1.0 in
          let fprob = if (List.length public >= 4) then fold_prob p
            else (fold_prob p) *. (fold_prob p) in
          if (randf <= fprob) then gen_winnerlst t acc
          else (
            let bh = best_hand (h1::h2::community) in
            gen_winnerlst t ((name p, bh)::acc)
          ) in
      let aibh = best_hand (c1::c2::community) in
      let winnerlst = gen_winnerlst holelst [("Hard AI", aibh)] in
      if (checkAI winnerlst) then
        simulation (n-1)
          (exp + (calc_money winnerlst (pot st + r * (List.length alive))
                  - calc_prev_investment - r)) r
      else simulation (n-1) (exp - calc_prev_investment - r) r
    ) in
  let minraise = if (min_raise st) = 0 then (pot st / 4) else min_raise st in
  let expf = (0 - calc_prev_investment) in
  let expc = simulation 1000 0 0 in
  let expr = simulation 1000 0 (2 * minraise) in
  if expf > expc && expf > expr then 0
  else if expc >= expf && expc >= expr then 1
  else 2


(* [heads_up st p] returns a command for an AI that is playing heads up.
   - requires: [st] is a valid state, and [p] is an AI player. *)
let heads_up st p =
  let min_raise =
    if (min_raise st = 0) then (pot st / 4) else (min_raise st) in
  match get_hand p with
  | None -> raise (Failure "Heads up AI has no card")
  | Some (c1,c2) -> (
      match stage st with
      | Deal -> (
          if (name (big_blind st) = name p) then (
            (* AI is big blind *)
            let action = if c1.num >= c2.num then three_bet_hdup c1 c2
              else three_bet_hdup c2 c1 in
            if (action = 2 && (to_call st p = 0)) then Call
            else if (action = 2) then Fold
            else if (action = 1) then Call
            else if (balance p >= (to_call st p) + 2 * min_raise) then
              Raise (2 * min_raise)
            else Call
          ) else (
            (* AI is button = dealer / small blind *)
            if (List.length (queue st) = 1) then
              (* not the openning *)
              if (min_raise = 50) then (* if the opponent min raises *)
                (if (balance p >= (to_call st p) + 2 * min_raise) then
                   Raise (2 * min_raise)
                 else Call)
              else (
                let action = if c1.num >= c2.num then three_bet_hdup c1 c2
                  else three_bet_hdup c2 c1 in
                if (action = 2 && (to_call st p = 0)) then Call
                else if (action = 2) then Fold
                else if (action = 1) then Call
                else if (balance p >= (to_call st p) + 2 * min_raise) then
                  Raise (2 * min_raise)
                else Call
              )
            else (
              (* openning *)
              if (c1.suit = c2.suit)
              then (if (balance p >= (to_call st p) + 2 * min_raise) then
                      Raise (2 * min_raise) else Call)
              else if (c1.num + c2.num >= 16 || c1.num = c2.num) then
                (if (balance p >= (to_call st p) + 2 * min_raise) then
                   Raise (2 * min_raise) else Call)
              else Fold
            )
          )
        )
      | Flop | Turn | River -> (
          let monte = montecarlo (public_hand st) c1 c2 in
          if (monte = true) then (
            (* nice chance of winning *)
            if (balance p < (to_call st p) + 2 * min_raise) then Call
            else if (min_raise >= pot st) then Call
            else Raise (2 * min_raise)
          ) else (
            (* chances of winning are low *)
            if (to_call st p = 0) then Call else Fold
          )
        )
      | Showdown -> raise (Failure "should not prompt for move in Showdown")
      | Empty -> failwith "ty"    )


(* [easy_move st p] returns a command for the easy AI given state [st].
   - requires: [st] is a valid state and [p] is an easy AI player. *)
let easy_move st p =
  match get_hand p with
  | None -> raise (Failure "Easy AI has no card")
  | Some (c1, c2) ->
    match stage st with
    | Deal -> (
        let score = hutchison_pair c1 c2 in
        if (score >= 31) then Call
        else if (score >= 27) then Call
        else if (to_call st p = 0) then Call
        else Fold
      )
    | Flop | Turn -> (
        let public = public_hand st in
        let best_hand = best_hand (c1::c2::public) in
        match best_hand with
        | High_Card, lst ->
          let outs = List.length (outs (c1::c2::public) st) in
          let outs_odd = to_percentage outs (List.length (deck st)) in
          let to_call = to_call st p in
          let earn = outs_odd * (pot st) - (100 - outs_odd) * to_call in
          if (earn > 0) then Call
          else if (to_call = 0) then Call
          else Fold
        | _,_-> Call
      )
    | River -> (
        let public = public_hand st in
        let best_hand = best_hand (c1::c2::public) in
        match best_hand with
        | High_Card, lst ->
          if (to_call st p = 0) then Call
          else Fold
        | _,_ -> Call
      )
    | Showdown -> raise (Failure "should not prompt for move in Showdown")
    | Empty -> failwith "epty"


(* [medium_move st p] returns a command for the medium AI given state [st].
   - requires: [st] is a valid state and [p] is an medium AI player. *)
let medium_move st p =
  if (List.length (alive st) = 2) then heads_up st p
  else
    match get_hand p with
    | None -> raise (Failure "Medium AI has no card")
    | Some (c1, c2) ->
      match stage st with
      | Deal -> (
          let score = hutchison_pair c1 c2 in
          if (score >= 31 && (balance p >= to_call st p + 2 * min_raise st))
          then Raise (2 * min_raise st)
          else if (score >= 31) then Call
          else if (score >= 27) then Call
          else if (to_call st p = 0) then Call
          else Fold
        )
      | Flop | Turn -> (
          let public = public_hand st in
          let best_hand = best_hand (c1::c2::public) in
          let cmd =
            if (min_raise st = 0 && balance p >= to_call st p + 100)
            then Raise 100
            else if (balance p >= to_call st p + min_raise st) then
              Raise (min_raise st)
            else Call in
          match best_hand with
          | Royal_Flush, lst -> cmd
          | Straight_Flush, lst -> cmd
          | Four_Kind, lst -> cmd
          | Full_House, lst -> cmd
          | Flush, lst -> cmd
          | Straight, lst -> cmd
          | Three_Kind, lst -> cmd
          | Two_Pair, lst -> cmd
          | Pair, lst -> cmd
          | High_Card, lst ->
            let outs = List.length (outs (c1::c2::public) st) in
            let outs_odd = to_percentage outs (List.length (deck st)) in
            let to_call = to_call st p in
            let earn = outs_odd * (pot st) - (100 - outs_odd) * to_call in
            if (earn > 0) then Call
            else if (to_call = 0) then Call
            else Fold
        )
      | River -> (
          let public = public_hand st in
          let best_hand = best_hand (c1::c2::public) in
          let cmd =
            if (min_raise st = 0 && balance p >= to_call st p + 100)
            then Raise 100
            else if (balance p >= to_call st p + min_raise st) then
              Raise (min_raise st)
            else Call in
          match best_hand with
          | Royal_Flush, lst -> cmd
          | Straight_Flush, lst -> cmd
          | Four_Kind, lst -> cmd
          | Full_House, lst -> cmd
          | Flush, lst -> cmd
          | Straight, lst -> cmd
          | Three_Kind, lst -> cmd
          | Two_Pair, lst -> cmd
          | Pair, lst -> Call
          | High_Card, lst ->
            if (to_call st p = 0) then Call
            else Fold
        )
      | Showdown -> raise (Failure "should not prompt for move in Showdown")
      | Empty -> failwith "emty"


(* [hard_move st p] returns a command for the hard AI given state [st].
   - requires: [st] is a valid state and [p] is an hard AI player. *)
let hard_move st p =
  if (List.length (alive st) = 2 && stage st = Deal) then heads_up st p
  else
    let min_raise =
      if (min_raise st = 0) then (pot st / 4) else (min_raise st) in
    match get_hand p with
    | None -> raise (Failure "Hard AI has no card")
    | Some (c1,c2) -> (
        match stage st with
        | Deal -> (
            if (name (dealer st) = name p) then (
              (* AI is dealer *)
              let move = if c1.num >= c2.num then dealer_range_loose c1 c2
                else dealer_range_loose c2 c1 in
              match move with
              | Red ->
                if (balance p >= (to_call st p) + 2 * min_raise)
                then Raise (2 * min_raise) else Call
              | Yellow ->
                if (balance p >= (to_call st p) + 2 * min_raise) && min_raise <= 50
                then Raise (2 * min_raise) else Call
              | Green ->
                if min_raise <= 50 then
                  if (balance p >= (to_call st p) + 2 * min_raise)
                  then Raise (2 * min_raise)
                  else Call
                else Fold
              | White -> if (to_call st p = 0) then Call else Fold
            ) else if (name (big_blind st) = name p) then (
              (* AI is big blind *)
              if min_raise > 50 then (
                (* vs. a raise *)
                let move = if c1.num >= c2.num then blind_vs_raise_loose c1 c2
                  else blind_vs_raise_loose c2 c1 in
                match move with
                | Red -> if (balance p >= (to_call st p) + 2 * min_raise)
                  then Raise (2 * min_raise) else Call
                | Yellow -> Call
                | _ -> if (to_call st p) = 0 then Call else Fold
              ) else (
                (* vs. a limper *)
                let move = if c1.num >= c2.num then blind_vs_limper_loose c1 c2
                  else blind_vs_limper_loose c2 c1 in
                match move with
                | Green -> if (balance p >= (to_call st p) + 2 * min_raise)
                  then Raise (2 * min_raise) else Call
                | _ -> Call
              )
            ) else if (name (small_blind st) = name p) then (
              (* AI is small blind *)
              if min_raise > 50 then (
                (* vs. a raise *)
                let move = if c1.num >= c2.num then blind_vs_raise_loose c1 c2
                  else blind_vs_raise_loose c2 c1 in
                match move with
                | Red -> if (balance p >= (to_call st p) + 2 * min_raise)
                  then Raise (2 * min_raise) else Call
                | Yellow -> Call
                | _ -> if (to_call st p) = 0 then Call else Fold
              ) else (
                (* vs. a limper *)
                let move = if c1.num >= c2.num then blind_vs_limper_loose c1 c2
                  else blind_vs_limper_loose c2 c1 in
                match move with
                | Green -> if (balance p >= (to_call st p) + 2 * min_raise)
                  then Raise (2 * min_raise) else Call
                | _ -> if (to_call st p) = 0 then Call else Fold
              )
            ) else (
              let move = if c1.num >= c2.num then cutoff_range_loose c1 c2
                else cutoff_range_loose c2 c1 in
              match move with
              | Red ->
                if (balance p >= (to_call st p) + 2 * min_raise)
                then Raise (2 * min_raise) else Call
              | Yellow ->
                if (balance p >= (to_call st p) + 2 * min_raise) && min_raise <= 50
                then Raise (2 * min_raise) else Call
              | Green ->
                if min_raise <= 50 then
                  if (balance p >= (to_call st p) + 2 * min_raise)
                  then Raise (2 * min_raise)
                  else Call
                else Fold
              | White -> if (to_call st p = 0) then Call else Fold
            )
          )
        | Flop | Turn | River -> (
            let move = multimonte st c1 c2 in
            if move = 0 && (to_call st p = 0) then Call
            else if move = 0 then Fold
            else if move = 1 then Call
            else
            if (balance p >= (to_call st p) + 2 * min_raise) then
              Raise (2 * min_raise) else Call
          )
        | Showdown -> raise (Failure "should not prompt for move in Showdown")
        | Empty -> failwith "empty"
      )
