(* GUI module for making the Graphic and user interaction *)
open Notty
open Notty_unix
open Ai
open Gamestate
open Player

let hd (a,_) = a

let tl (_,b) = b

(* Creates a Notty string with it's contents being string s *)
let img s = I.string A.empty s

(* Centers an image. Taken from 3110 Fall 2017 Camlization project for it's
   simple and wide-reaching uses *)
let center w h i = I.vsnap h (I.hsnap w i)

let square = "\xe2\x96\xaa"

(* returns an Image rectangle of height h and width w with color c*)
let rectangle_color h w c =
  let l1 = I.hcat [
      I.uchar c (Uchar.of_int 0x2503) 1 h;
    ] in
  let w1 = I.uchar c (Uchar.of_int 0x2501) w 1 in
  I.vcat [
    I.(string c "┏" <|> w1 <|> string c "┓");
    I.(l1 <|> I.void w h <|> l1);
    I.(string c "┗" <|> w1 <|> string c "┛");
  ]

(* returns an Image rectangle of height h and width w *)
let rectangle h w =
  let l1 = I.hcat [
    I.uchar A.empty (Uchar.of_int 0x2503) 1 h;
    ] in
  let w1 = I.uchar A.empty (Uchar.of_int 0x2501) w 1 in
  I.vcat [
    I.(img "┏" <|> w1 <|> img "┓");
    I.(l1 <|> I.void w h <|> l1);
    I.(img "┗" <|> w1 <|> img "┛");
  ]

(* creates an image of a card with height and width 7. Draws in their
   suit and number as well *)
let card_drawn n s =
  let number n =
    if n = 11 then "J"
    else if n = 12 then "Q"
    else if n = 13 then "K"
    else if n = 14 then "A"
    else if n > 14 || n < 1 then "none"
    else string_of_int n
  in let n2 = number n in
  let cardw = 7 in let cardh = 7 in
  let rect = rectangle cardw cardh in
  match s with
  |Spade -> let top = I.(hcat [
      void 2 1;
      uchar A.empty (Uchar.of_int 0x2660) 1 1;
      void (cardw / 2) (cardh - 3);
      string A.(fg black) (n2);
    ] |> vpad 1 2)
    in let bottom = I.(hcat [
      void 2 1;
      string A.(fg black) (n2);
      void (cardw / 2) (cardh - 3);
      uchar A.empty (Uchar.of_int 0x2660) 1 1;
    ])
    in let sign = I.(top <-> bottom) in
    if n2 <> "none" then I.(sign </> rect) else I.(rect)
  |Heart -> let top = I.(hcat [
      void 2 1;
      uchar A.(fg red) (Uchar.of_int 0x2665) 1 1;
      void (cardw / 2) (cardh - 3);
      string A.(fg black) (n2);
    ] |> vpad 1 2)
    in let bottom = I.(hcat [
        void 2 1;
        string A.(fg black) (n2);
        void (cardw / 2) (cardh - 3);
        uchar A.(fg red) (Uchar.of_int 0x2665) 1 1;
      ])
    in let sign = I.(top <-> bottom) in
    if n2 <> "none" then I.(sign </> rect) else I.(rect)
  |Diamond -> let top = I.(hcat [
      void 2 1;
      uchar A.(fg red) (Uchar.of_int 0x2666) 1 1;
      void (cardw / 2) (cardh - 3);
      string A.(fg black) (n2);
    ] |> vpad 1 2)
    in let bottom = I.(hcat [
        void 2 1;
        string A.(fg black) (n2);
        void (cardw / 2) (cardh - 3);
        uchar A.(fg red) (Uchar.of_int 0x2666) 1 1;
      ])
    in let sign = I.(top <-> bottom) in
    if n2 <> "none" then I.(sign </> rect) else I.(rect)
  |Club -> let top = I.(hcat [
      void 2 1;
      uchar A.empty (Uchar.of_int 0x2663) 1 1;
      void (cardw / 2) (cardh - 3);
      string A.(fg black) (n2);
    ] |> vpad 1 2)
    in let bottom = I.(hcat [
        void 2 1;
        string A.(fg black) (n2);
        void (cardw / 2) (cardh - 3);
        uchar A.empty (Uchar.of_int 0x2663) 1 1;
      ])
    in let sign = I.(top <-> bottom) in
    if n2 <> "none" then I.(sign </> rect) else I.(rect)

(* calls card_drawn with the correct suit/number. Returns a Notty Image *)
let draw_card card =
  match card with
  |{num = n; suit = s} -> card_drawn n s

(* The 3 buttons on the GUI that can be pressed. Each button has an index that
   shows which button the player should be on (0 is first button, 1 is second,
   2 is third.) and an img which is the img that should be at that button. *)
type interface_item = {
    index: int;
    img: image;
  }

(* Takes in the current player index, the "options" (which is how much a players
   intends to raise by) and an array of Notty attributes. It then creates the
   buttons that are to be used by the player. If a player is at the selected
   button, it draws the rectangle as a different color. Return an array of
   interface items.*)
let interface_items (i, options, colors) = [|
  {
    index = 0;
    img = I.(zcat [
      center 13 5 (string A.(fg yellow) "CHECK/CALL");
      rectangle_color 3 11 colors.(0);
    ])
  };
  {
    index = 1;
    img = I.(zcat [
        center 13 5 ((string A.(fg green) "RAISE") |> pad ~l:(2) <->
                (string A.(fg green) ("❮Q " ^ string_of_int options ^ " E❯")));
        rectangle_color 3 11 colors.(1);
      ])
  };
  {
    index = 2;
    img = I.(zcat [
        center 13 5 (string A.(fg yellow) " FOLD");
        rectangle_color 3 11 colors.(2);
      ])
  }
|]

(* Takes in the current player index and how much the player currently intends
   to raise. Depending on what the index is, it creates the color array that
   will highlight the rectangle that the player index is at. Draws all the
   buttons from the array and connects them all, then returns them as a
Notty Image*)
let interface ind option =
  let colors = if ind = 1 then [|A.(fg magenta); A.empty; A.empty|]
    else if ind = 2 then [|A.empty; A.(fg magenta); A.empty|]
    else if ind = 3 then [|A.empty;A.empty;A.(fg magenta)|]
    else [|A.empty;A.empty;A.empty|] in
  let items = Array.map (fun i -> i.img) (interface_items (1, option, colors)) in
  let w = Array.fold_left (fun a i -> a + (I.width i)) 0 items + 2 in
  let h = Array.fold_left (fun a i -> max a (I.height i)) 0 items in
  let itemstogether = I.(hcat [items.(0); items.(1); items.(2)]
                          |> vpad 1 2 |> hpad 2 1) in
  I.(rectangle h w </> itemstogether)

(* A helper function that returns the correct element i from a list li. The
element i must be inside the list li otherwise the function fails. *)
let rec get_ele_from_li i li =
  match li with
  |[] -> failwith "wrong"
  |h::t -> if i = 0 then h else get_ele_from_li (i-1) t

(* Creates an array of all the players given to it. This array has the actual
 player type, not their names. *)
let array_of_player_lst players =
  let a = Array.make (List.length players) (init_player "whackaslkdjf" None) in
  let make_array =
    for i = 0 to (List.length players - 1) do
      a.(i) <- get_ele_from_li i players
    done
  in make_array; a

(* Creates the notty image of the "right board". This includes the names of the
   players, their current balance, the current Pot, and the current Call amount.
   Takes in a list of all the players (of player type) the current Pot and the
   current Call. Returns a Notty Image that has all the text information attached
 to each other.*)
let init_right_board players pot call =
  let a = array_of_player_lst players in
  let imga = Array.make (List.length players + 2) (img "") in
  let make_img_array =
    for i = 0 to (Array.length a) - 1 do
      imga.(i) <- I.(string A.(bg lightcyan) a.(i).name
                     <-> string A.(fg black) (string_of_int a.(i).balance))
    done
  in let add_pot =
       imga.(Array.length imga - 2) <- I.(string A.(bg lightcyan) "POT"
                                          <->
                                          string A.(fg black)
                                            (string_of_int pot))
  in let add_call =
       imga.(Array.length imga - 1) <- I.(string A.(bg lightcyan) "CALL"
                                          <-> string A.(fg black)
                                            (string_of_int (call)))
  in make_img_array; add_pot; add_call; imga

(* Takes in the current gamestate. Draws a rectangle around the image returned
from init_right_board. *)
let right_board gst =
  let items =
    init_right_board (players gst)
      (pot gst) (to_call gst (current_player gst |> getplayer (players gst))) in
  let w = Array.fold_left (fun a i -> max a (I.width i)) 0 items + 4 in
  let h = Array.fold_left (fun a i -> a + (I.height i)) 0 items + 2 in
  let rec itemstogether i =
    if i = Array.length items - 1 then
      items.(i)
    else
      I.(items.(i) <-> itemstogether (i+1))
  in I.(rectangle h w </> (itemstogether 0 |> pad ~l:2 ~t:2))

(* Takes in the list of the public_hand and an incrementor i which tracks at
   which of the five public hand cards the drawing function is currently at.
   This function recursively draws a card and appends it to the other public
   hand cards. It then returns the image of all the cards combined. *)
let rec draw_public_hands public_hand i =
  let blank = draw_card ({num = 15; suit = Spade}) in
  if i = 4 then try draw_card (get_ele_from_li i public_hand) with _ -> blank
  else try I.(draw_card (get_ele_from_li i public_hand)
              <|> draw_public_hands public_hand (i + 1)) with _ ->
    I.(blank <|> draw_public_hands public_hand (i + 1))

(* Takes in the player index ind and the current gamestate gst. For each player
   it then draws their card in 2 cases only: if it's their turn and they're not
   an AI, or if the round has won and there is a winner, in which case everyone
   has their hand revealed. Returns the image of the cards appended together*)
let rec draw_player_cards ind gst =
  let blank_card = draw_card ({num = 15; suit = Spade}) in
  let w = (I.width blank_card) * 2 + 2 in
  let h = (I.height blank_card) + 1 in
  let blank = ({num = 15; suit = Spade}) in
  let a = array_of_player_lst (players gst) in
  let imga = Array.make (List.length (players gst) + 1) (img "") in
  let make_img_array =
    for i = 0 to (Array.length a) - 1 do
      let hand = (get_hand a.(i)) in
      let card1 = match hand with
        | None -> blank
        | Some (c1, _) -> if (a.(i).name = (current_player gst) &&
                             (current_player_type gst) = None &&
                             ind <> 4) || ind = 5
          then c1 else blank
      in
      let card2 = match hand with
        | None -> blank
        |Some (_, c2) -> if (a.(i).name = (current_player gst) &&
                            (current_player_type gst) = None &&
                             ind <> 4) || ind = 5
          then c2 else blank
      in
      if (a.(i).name) = (dealer gst |> name) then
        imga.(i) <- I.(rectangle h w </>
                       (string A.(bg lightcyan) ("(⌐■_■)" ^ a.(i).name)
                        |> pad ~l:(1)
                           <-> (draw_card card1 <|> draw_card card2)
                        |> pad ~l:2 ~t:1))
      else
      imga.(i) <- I.(rectangle h w </>
                     (string A.(bg lightcyan) a.(i).name
                      |> pad ~l:(1)
                        <-> (draw_card card1 <|> draw_card card2)
                     |> pad ~l:2 ~t:1))
    done
  in make_img_array; imga

(* Returns the public hand image *)
let init_public_hand stage public_hand =
  draw_public_hands public_hand 0

(* Creates the "left_board" of the game. This includes the public hand, the
   player hands, and the box that goes around it. Takes in the player index
   and the current gamestate. Returns the rectangle with all the correct
elements inside of it. *)
let left_board ind gst =
  let items = draw_player_cards ind gst in
  let itemswidth = Array.fold_left (fun a i -> a + (I.width i)) 0 items + 4 in
  let public = init_public_hand (players gst) (public_hand gst) in
  let w = max (itemswidth)
      (I.width public + 4) in
  let h = Array.fold_left (fun a i -> max a (I.height i)) 0 items +
          I.height public + 2 in
  let rec itemstogether i =
    if i = Array.length items - 1 then
      items.(i)
    else
      I.(items.(i) <|> itemstogether (i+1))
  in I.(rectangle h w </> (public
                           |> pad ~l:((w/2) - (I.width public)/2 + 1) ~t: 1
            <-> (itemstogether 0 |> pad ~l:((w/2) - (itemswidth)/2 + 2) ~t:1)))

(* Draws the gamestate message at the button inside of a rectangle. The
   rectangle is designed to be slightly bigger than the interface, so that the
image is always big enough for the message. Returns the image *)
let bottom gst =
  let mes = I.(string A.(fg yellow) (show_message gst))  in
  let w = I.width (interface 1 1) *3 in
  I.((mes) |> pad ~l:(w/2 - I.width(mes)/2) ~t:2
              </> rectangle 4 (w))

(* draws the entire board, including left, right, interface, and bottom.
Returns the image that's the entire board together.  *)
let draw_board i option gst =
  let left = left_board i gst in
  let right = right_board gst in
  let inter = interface i option in
  let bottom = bottom gst in
  let w = max (I.width left) (I.width bottom)
          + I.width right + 2 in
  let h = (I.height left) +
          (I.height (inter)) + (I.height bottom) + 2 in
  I.((rectangle h w
      </> vcat [
        left <|> right |> pad ~t:1
          ~l:(w/2 - (I.width left/2+I.width right/2) + 1);
        (inter |> pad ~t:1 ~l:(w/2 - I.width (inter)/2 + 1));
        bottom |> pad ~l:(w/2 - I.width (bottom)/2 + 1)
      ]))

(* Draws the stage inbetween turns where it tells the user who's turn it is.
If it's currently an AI's turn, the correct message will warn the player.  *)
let draw_inbetween gst =
  let mes = if (current_player_type gst <> None) then
      "It is " ^ current_player gst ^ "'s turn. This is an AI. Their move "
      ^ " will be played. Press enter to see their move, then enter again to " ^
      " continue. "
    else "It is " ^ current_player gst ^ "'s turn. Press enter when ready"
  in I.(string A.empty mes)

(* takes the current player index, the raise amount, the open terminal, and the
   game state. Loops recursively waiting by first drawing the correct image onto
   the terminal then waiting for a player input. Depending on the input, it'll
jump to a new stage. Escape should always quit the game*)
let rec gmain (i,option) t gst =
  if i = 0
  then Term.image t (draw_inbetween gst)
  else Term.image t (draw_board i option gst);
  match Term.event t with
    |`Key (`Escape, []) -> ()
    |`Key (`ASCII c, []) ->
      if c = 'q' then
      if i <> 2 then gmain (i,option) t gst
      else if option - 50 < (min_raise gst)
      then let x = (current_player gst
                    |> getplayer (players gst) |> balance) -
                   to_call gst (current_player gst
                                |> getplayer (players gst)) in
        if x < 0 then gmain (2,0) t gst else
        gmain (2, (x)) t gst
      else gmain (2,option-50) t gst
    else if c = 'e' then
      if i <> 2 then gmain (i,option) t gst
      else if option + 50 > ((current_player gst
                              |> getplayer (players gst) |> balance) -
                             to_call gst (current_player gst
                                          |> getplayer (players gst)))
      then gmain (2, (min_raise gst)) t gst
      else gmain (2,option+50) t gst
    else gmain (i,option) t gst
    |`Key (`Arrow(`Left), []) -> if i = 1 then gmain (1,option) t gst
      else if i > 3 then gmain (i,option) t gst
      else if i = 2 || i = 3 then gmain (i-1,option) t gst
      else gmain (1, option) t gst
    |`Key (`Arrow(`Right), []) -> if i = 3 then gmain (3,option) t gst
      else if i > 3 then gmain (i,option) t gst
      else if i = 1 || i = 2 then gmain (i+1, option) t gst
      else gmain (3, option) t gst
    |`Key (`Enter, []) ->
         if i = 0 &&
                   Str.string_match (Str.regexp "Game W") (show_message gst) 0 then
             gmain (6,option) t gst
      else if stage gst = Showdown && i <> 6 then
        gmain (5, option) t (win_state gst |> hd)
      else if i = 5 then
        gmain (0,option) t (end' {gst with stage = Showdown})
      else if current_player_type gst <> None && i <> 6 then
        if (next_player gst |> getplayer (players gst) |> player_type) <> None
        then
          match (current_player_type gst) with
          | Hard -> gmain (4, option) t (do' (hard_move gst (getplayer (queue gst)
                                                               (current_player gst))) gst)
          | Medium -> gmain (4, option) t (do' (medium_move gst (getplayer (queue gst)
                                                                   (current_player gst))) gst)
          | Easy -> gmain (4, option) t (do' (easy_move gst (getplayer (queue gst)
                                                               (current_player gst))) gst)
          | None -> failwith "shouldn't happen"
        else
          match (current_player_type gst) with
          | Hard -> gmain (0, option) t (do' (hard_move gst (getplayer (queue gst)
                                                               (current_player gst))) gst)
          | Medium -> gmain (0, option) t (do' (medium_move gst (getplayer (queue gst)
                                                                   (current_player gst))) gst)
          | Easy -> gmain (0, option) t (do' (easy_move gst (getplayer (queue gst)
                                                               (current_player gst))) gst)
          | None -> failwith "sholudn't happen"
      else if i = 0 then gmain (1,option) t gst
      else if i = 3 then
        gmain (0,option) t (do' Fold gst)
      else if i = 1 then
        gmain (0,option) t (do' Call gst)
      else if i = 2 then
        gmain (0, 50) t (do' (Raise option) gst)
      else if i = 4 then
        gmain (0,option) t (gst)
      else
        gmain (i,option) t (gst)
    |`End -> ()
    |_ -> gmain (i,option) t gst
