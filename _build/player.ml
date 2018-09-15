type suit = Spade | Heart | Diamond | Club
type ai_type = None | Easy | Medium | Hard
type card =
  {
  num : int;
  suit : suit;
}

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

let init_player str ai =
  {is_ai = ai; name = str; balance = 5300;
   hand = None; folded = false; all_in = false;
  f_numer = 0; denom = 0; c_numer = 0;}

let player_type p = p.is_ai

let name p = p.name

let folded p = p.folded

let get_hand p = p.hand

let string_of_hand p =
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
  in match p.hand with
  | None -> "None"
  | Some (c1, c2) -> (string_of_num c1.num) ^ " of " ^ (string_of_suit c1.suit) ^ "\n          " ^
                     (string_of_num c2.num) ^ " of " ^ (string_of_suit c2.suit)

let balance p = p.balance

let reset_player p = {p with hand = None; folded = false; all_in = false}

let all_in p = p.all_in

let set_all_in p b =
  {p with all_in = b}

let set_balance p new_bal =
  {p with balance = new_bal}

let set_hand p (a,b) = {p with hand = Some (a,b)}

let change_folded p = {p with folded = not(p.folded)}

let fold_prob p =
  if p.denom = 0 then float_of_int p.f_numer /. 1.0 else
  float_of_int p.f_numer /. float_of_int p.denom

let call_prob p =
  if p.denom = 0 then float_of_int p.c_numer /. 1.0 else
    float_of_int p.c_numer /. float_of_int p.denom

let raise_prob p =
  1.0 -. (fold_prob p) -. (fold_prob p)
