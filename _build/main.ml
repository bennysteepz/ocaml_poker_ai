open Gamestate
open Command
open Gui
open Notty
open Notty_unix
open Pervasives
open Player
open Ai
open Menu

(* prompt for commands *)
let rec read_commands (st : state) =
  let message = show_message st in
  if message = "Exiting..."
  then print_string ("\n" ^ message ^ "Done.\nThanks for playing!\n")
  else begin
    let () = ANSITerminal.(print_string [red] ("\n" ^ message ^ "\n \n")) in
    match current_player_type st with
    | Hard -> read_commands (do' (hard_move st (getplayer (queue st)
                                                  (current_player st))) st)
    | Medium -> read_commands (do' (medium_move st (getplayer (queue st)
                                                      (current_player st))) st)
    | Easy -> read_commands (do' (easy_move st (getplayer (queue st)
                                                  (current_player st))) st)
    | None -> begin
      let () = ANSITerminal.(print_string [green] ">>:") in
    match read_line () with
    | exception End_of_file -> ANSITerminal.(print_string [yellow] "\nSorry, what? \n")
    | s -> read_commands (do' (parse s) st)
  end
  end

(* initialize the player given the input string [s] *)
let initialize_players s =
  let rec ai_help num difficulty acc : player list=
    if num = 0 then acc
    else match difficulty with
      | Hard -> ai_help (num-1) Hard ((init_player ("Hard AI " ^ string_of_int num)
                                         Hard) :: acc)
      | Medium -> ai_help (num-1) Medium ((init_player ("Medium AI " ^ string_of_int num)
                                            Medium) :: acc)
      | Easy -> ai_help (num-1) Easy ((init_player ("Easy AI " ^ string_of_int num)
                                          Easy) :: acc)
      | None -> failwith "this should never be called."
  in let initialize_ai s acc =
    let first_comma = String.index_opt s ',' in
    match first_comma with
    | None -> acc
    | Some i -> begin
        let easy_ai = String.trim (String.sub s 0 i) in
        let remaining = String.sub s (i+1) (String.length s - i-1)
        in let second_comma = String.index_opt remaining ',' in
        match second_comma with
        | None -> acc
        | Some i -> begin
            let medium_ai = String.trim (String.sub remaining 0 i) in
            let hard_ai = String.trim (String.sub remaining (i+1)
                                         (String.length remaining - i-1)) in
            let num_easy = try (let n = int_of_string easy_ai in
                                if n >= 0 then n else 0) with | _ -> 0 in
            let num_medium = try (let n = int_of_string medium_ai in
                                if n >= 0 then n else 0) with | _ -> 0 in
            let num_hard = try (let n = int_of_string hard_ai in
                                if n >= 0 then n else 0) with | _ -> 0 in
            (acc |> ai_help num_easy Easy |>
             ai_help num_medium Medium |> ai_help num_hard Hard)
          end
    end
  in let rec initialize_humans s acc =
    if String.trim s = "" then acc
    else let comma_ind = String.index_opt s ',' in
      match comma_ind with
      | Some i -> begin
          let p_id = String.trim (String.sub s 0 i) in
          let player_id = if String.length (p_id) > 12
                      then String.sub (p_id) 0 12
                      else p_id in
          if String.length (player_id) >= 7
          then if String.sub player_id 0 7 = "Easy AI"
            then failwith "Stop pretending to be an AI!"
            else if String.sub player_id 0 7 = "Hard AI"
            then failwith "Stop pretending to be an AI!"
            else if String.length (player_id) >= 9
            then if String.sub player_id 0 9 = "Medium AI"
              then failwith "Stop pretending to be an AI!"
              else let s' = String.trim (String.sub s (i+1) (String.length s-i-1)) in
                let new_player = (init_player player_id None) in
                if List.mem new_player acc then initialize_humans s' acc
                else initialize_humans s' (new_player :: acc)
            else let s' = String.trim (String.sub s (i+1) (String.length s-i-1)) in
              let new_player = (init_player player_id None) in
              if List.mem new_player acc then initialize_humans s' acc
              else initialize_humans s' (new_player :: acc)
          else let s' = String.trim (String.sub s (i+1) (String.length s-i-1)) in
            let new_player = (init_player player_id None) in
            if List.mem new_player acc then initialize_humans s' acc
            else initialize_humans s' (new_player :: acc)
    end
      | None -> begin
          let player_id = if String.length (String.trim s) > 12
                      then String.sub (String.trim s) 0 12
                      else String.trim s
          in
          if String.length (player_id) >= 7
          then if String.sub player_id 0 7 = "Easy AI"
            then failwith "Stop pretending to be an AI!"
            else if String.sub player_id 0 7 = "Hard AI"
            then failwith "Stop pretending to be an AI!"
            else if String.length (player_id) >= 9
            then if String.sub player_id 0 9 = "Medium AI"
              then failwith "Stop pretending to be an AI!"
              else let new_player = (init_player player_id None) in
          if List.mem new_player acc then acc
          else new_player :: acc
            else let new_player = (init_player player_id None) in
            if List.mem new_player acc then acc
            else new_player :: acc
          else let new_player = (init_player player_id None) in
          if List.mem new_player acc then acc
          else new_player :: acc
      end
  in let player_lst = (let period_ind = String.index_opt s '.' in
    match period_ind with
    | Some i -> let human_players = (initialize_humans (String.sub s 0 i) []) in
      if List.length (human_players) > 0
      then initialize_ai (String.sub s (i+1)
                            (String.length s -i-1)) human_players
      else failwith "You can't play this game with only AI!"
    | None -> initialize_humans s []) in
  (*This part throws exceptions when the entered string with player IDs does not
   * fit specified parameters. Might want to fix so that exceptions aren't thrown,
   * and warnings are printed instead.*)
  if List.length player_lst < 2
  then let () = ANSITerminal.(print_string [red] "Not enough players. Exiting.\n")
    in failwith ""
  else if List.length player_lst > 6
  then let () = ANSITerminal.(print_string [red] "Too many players. Exiting.\n")
    in failwith ""
  else  let () = ANSITerminal.(print_string [red]
                                 "Thank you. The game starts now! \n \n " )
(* How to use this REPL: \n
raise <int>                          -- raises the current player's bet by <int>, if permitted by game rules.\n
call                                 -- current player elects to follow the current bet.\n
fold                                 -- current player forfeits the round.\n
show                                 -- shows the current player's hand, given a round is currently taking place.\n
status                               -- shows the stats of the current round and overall game.\n
manual                               -- prints out instructions on using this REPL (the instructions written here)\n
           exit                                 -- exits the game.\n ) *)

    in player_lst

(* runs the gui *)
let run t (st: overallstate) =
  match st with
  |Menu gst -> menu t gst
  |Game gst -> gmain (0,(min_raise gst)) t gst
  |Quit -> ()

(* starts the whole game *)
let rec main () : unit =
ANSITerminal.(print_string [red]
                "\n\nWelcome to 3110 Text-Based Texas Hold'Em Poker!\n \n
PLEASE MAXIMIZE YOUR TERMINAL, THE GAME WILL NOT WORK OTHERWISE.
There must be at least two players but less than six
players in this game. \n \n
Before starting the game, please enter the names of the
human players, each separated by a comma. Any names that
start with \"Easy AI\" or \"Medium AI\" or \"Hard AI\" are
not allowed. If you wish to have AI players in the game, enter
a period after the names of the human players, then indicate
how many AI players you want for each difficulty by typing in: \n
<number of Easy AI>, <number of Medium AI>, <number of Hard AI>.\n
Example entry: a,b,c,d. 0,1,0 \n
This will initialize four human players, named a,b,c, and d,
and one AI player, set to medium difficulty. \n \n
**When entering human player names, PLEASE make sure that
all names are unique. Exactly ONE player will be added to
the game for every unique player ID inputted here. Case
sensitivity is honored here, and any whitespace before or
after a player ID will be ignored. Also please ensure that
your player name is less than 12 characters long. If it's longer
than 12 characters it will be truncated.**\n \n");
print_string  "> ";
let gst =
  match read_line () with
  | exception _ -> failwith "readline"
  | s -> (init_state (initialize_players s))
in let t = Term.create() in
run t (Menu gst)


let () = main ()
