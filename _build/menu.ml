open Notty
open Notty_unix
open Gamestate
open Gui
open Player

let img s = I.string A.empty s

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

(* menu_item has an index that is it's index of the menu and the image that it
   intends to display. *)
type menu_item = {
  index: int;
  img: image;
}

(* menu_items takes in an array of Attriutes and creates an array of menu_item
   that is each part of the menu. Each object is a rectangle of size 5 by 40,
   and it's text color is the color specified by the colors array.*)
let menu_items colors = [|
  {
    index = 0;
    img = I.(zcat [
        center 40 8 ((string colors.(0) "START GAME"));
        rectangle 5 40;
      ]);
  };
  {
    index = 1;
    img = I.(zcat [
        center 40 8 ((string colors.(1) "CONTROLS"));
        rectangle 5 40;
      ]);
  };
  {
    index = 2;
    img = I.(zcat [
        center 40 8 ((string colors.(2) "ACKNOWLEDGEMENTS"));
        rectangle 5 40;
      ]);
  }
|]

(* Writes the message. Depending on input i (whether the player pressed conrols
   or acknowledgements, the message will be different) *)
let write_message i =
  if i = 3 then
    I.(string A.(fg green) "General Texas Hold 'Em Rules Apply. The dealer is the guy wearing the sunglasses."
       <-> string A.(fg green) "The Controls are:"
       <-> string A.(fg green) "Left and Right to choose your move"
       <-> string A.(fg green) ("Q and E when on the Raise button to select " ^
           "how much you intend to raise.")
       <-> string A.(fg green) "Enter to select your move or advance the game."
       <-> string A.(fg green) "At any point in the game, press esc to exit."
       <-> string A.(fg green) "To return to the main menu, press esc.")
  else if i = 4 then
    I.(string A.(fg green) ("This project was made by Eric Sun, Elaine Hwang, " ^
       "Benjamin Steeper, and Kristi Fok for our Spring 2018 CS 3110 Final Project.")
       <-> string A.(fg green) ("Special thanks to David Kaloper for his " ^
                                "graphics library Notty.")
       <-> string A.(fg green) ("To return to the main menu, press esc."))
  else I.string A.empty "Uh-oh"

(* Makes the color array. Depending on the player index (what button they're at)
 it will make one of the boxes have red text*)
let make_colarray i =
  if i = 0 then [|A.(fg red); A.empty; A.empty|]
  else if i = 1 then [|A.empty; A.(fg red); A.empty|]
  else if i = 2 then [|A.empty; A.empty; A.(fg red)|]
  else [|A.empty; A.empty; A.empty|]

(* recursively iterates until the player either starts the game or exits the
   menu. Draws the menu_items and displays them on the terminal, then waits
   for a player response. If the response is enter, depending on at what button
the player is at, it will draw different outputs. *)
let rec mmenu i t gst =
  let (w,h) = Term.size t in
  let items = menu_items (make_colarray i) in
  if i = 0 || i = 1 || i = 2 then
  Term.image t I.((items.(0).img <-> items.(1).img <-> items.(2).img)
                  |> pad ~l:(w/2 - (I.width items.(0).img) /2)
                    ~t:(h/2 - (I.height items.(0).img * 3)/2))
  else if i = 3 || i = 4 then
    Term.image t (write_message i);
  match Term.event t with
  |`Key (`Escape, []) ->
    if i = 3 || i = 4 then mmenu 0 t gst
    else ()
  |`Key (`Arrow(`Down), []) -> if i = 2
    then mmenu 2 t gst
    else mmenu (i+1) t gst
  |`Key (`Arrow(`Up), []) -> if i = 0
    then mmenu 0 t gst
    else mmenu (i-1) t gst
  |`Key (`Enter, []) ->
    if i = 0 then gmain (0, min_raise gst) t gst
    else if i = 1 then mmenu 3 t gst
    else if i = 2 then mmenu 4 t gst
    else mmenu 0 t gst
  |_ -> mmenu i t gst

let menu t gst = mmenu 0 t gst





















(*  *)
