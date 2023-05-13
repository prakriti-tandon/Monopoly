(*Open statements*)
open Game
open Command
open State
open Board
open Property

(*Global variable which is set at the time when we initialise the game*)
let num_player = ref 0
let game_end = ref false
let active_player = ref 0

(*ends the game and shows the name of the player that won*)
let terminate str =
  print_endline (str ^ " has won!");
  exit 0

(*calls once and returns a function*)
let rec roll_die =
  let _ = Random.self_init () in
  fun () ->
    let n = Random.int 7 in
    if n != 0 then n else roll_die ()

(*both helpers need to return a tupple of s1 and st2 where the first *)
let trans st1 st2 board = raise (Failure "Unimplemented")
(*try let comb_state = State.pay_rent st1 st2 board in (comb_state.player1,
  comb_state.player2) with InsufficientFunds -> terminate (State.name st2) *)

(*recursive function that loops through and annoys the user till they return
  either y or n as a value*)
let rec get_command () =
  match read_line () with
  | "y" -> "y"
  | "n" -> "n"
  | _ ->
      print_endline
        "Oh no thats not a valid command type y for yes and n for no";
      print_string "> ";
      get_command ()

let prompt_buy st1 st2 board =
  try
    let st_new = State.buy_property st1 (current_pos st1) board in
    (st_new, st2)
  with InsufficientFunds -> terminate (State.name st2)

(*runs through one player turn which involves changing states, paying rent etc*)
let player_run st1 st2 board =
  print_string "\n";
  print_endline ("It is player " ^ State.name st1 ^ "'s turn.");
  let n = roll_die () in
  let st1_go = State.go n st1 board in
  print_endline ("You rolled a " ^ string_of_int n ^ "!");
  (*check if current spot is a property*)
  let curr_pos = current_pos st1_go in
  (*print description and name of the property and check for owner *)
  print_endline (State.name st1 ^ " landed on " ^ name board curr_pos ^ ":");
  print_endline (description board curr_pos);
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("Your current balance is "
    ^ string_of_int (State.current_balance st1)
    ^ ".");
  print_endline " ";
  (*do all the matching only if the state is property otherwise just return
    (st1_go, st2)*)
  if space_type board (State.current_pos st1_go) = "property" then
    match State.owns st2 curr_pos board with
    | true ->
        (*here the first argument is the player who is paying and second arg is
          the one who is receiving*)
        print_endline
          ("You paid rent of "
          ^ string_of_int (rent board curr_pos)
          ^ " to the property owner.");
        trans st1_go st2 board
    | false ->
        (* if current player already owns the space, don't prompt them to buy *)
        if owns st1_go curr_pos board then (
          print_endline "You already own this space.";
          (st1_go, st2))
        else (
          print_endline
            ("The price to buy is " ^ string_of_int (price board curr_pos) ^ ".");
          print_endline "Buy? [y/n]";
          print_string ">";
          let str = get_command () in
          match str with
          | "y" -> (*prompt_buy st1_go st2 board*) (st1_go, st2)
          | "n" -> (st1_go, st2)
          | _ -> (st1_go, st2))
  else (st1_go, st2)

(*main game loop*)
let rec game_loop st1 st2 board =
  (*note-to-self: termination is handled in the transaction functions not here
    -> hard to deal with the exceptions in the main loop*)
  (*the first argument put into player_run is the player whose turn it is right
    now*)
  let state_after1 = player_run st1 st2 board in
  match state_after1 with
  (*deals with player 2's turn and then recursively calls game_loop thereby
    starting player 1's turn again*)
  | x, y ->
      let state_after_2 = player_run y x board in
      game_loop (snd state_after_2) (fst state_after_2) board

(* name_helper and get_name are funtions used to get the list of player names
   out*)
let rec name_helper str =
  print_endline
    "Oops! The number of player names and actual players don't match -> try \
     again! ";
  print_string "> ";
  get_name ()

and get_name () =
  print_endline
    "Please enter the name of all the players: (name of first player, followed \
     by space and then name of the second player and so on...) ";
  print_string "> ";
  match read_line () with
  | str ->
      let lst = String.split_on_char ' ' (str ^ " ") in
      if List.length lst = !num_player then name_helper () else lst
(* | exception End_of_file -> ()*)

let update_active (arr : player_list) (cp : int) =
  let np = (cp + 1) mod !num_player in
  active_player := np

let ask_buy () = ()
let self_own_prompt () = ()

let deal_property board arr int =
  let active_p = arr.(int) in
  let curr_pos = current_pos active_p in
  match property_status arr active_p board with
  | OwnedByOtherPlayer st -> ()
  | NotOwned -> ask_buy ()
  | OwnedByThisPlayer -> self_own_prompt ()

let deal_go board arr int = raise (Failure "unimp")
let deal_jail board arr int = raise (Failure "unimp")

(* This function chooses a chance card for the player, executes the chance card,
   then updates who the active player is. This NEEDS TO BE UPDATED to handle the
   case where a card bankrupts a player. *)
let deal_card board arr deck bank exec_fn int =
  let active_p = arr.(int) in
  let chosen_card = Deck.random_card deck () in
  print_endline ("You drew " ^ Deck.name deck chosen_card ^ "!");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (Deck.description deck chosen_card);
  exec_fn deck arr bank board chosen_card active_p;
  update_active arr int

let deal_chance board arr chance_deck bank int =
  deal_card board arr chance_deck bank Chance.exec_card int

let deal_commchest board arr comm_deck bank int =
  deal_card board arr comm_deck bank Comm_chest.exec_card int

let rec multi_player_run board arr chance_deck comm_deck bank int =
  let active_p = arr.(int) in
  print_string "\n";
  print_endline ("It is player " ^ State.name active_p ^ "'s turn.");
  let n = roll_die () in
  (*active_p_new is the updated state on moving the active player*)
  let active_p_new = State.go n active_p board in
  (*updating the state in the array*)
  arr.(int) <- active_p_new;
  print_endline ("You rolled a " ^ string_of_int n ^ "!");
  (*check if current spot is a property*)
  let curr_pos = current_pos active_p_new in
  (*print description and name of the property and check for owner *)
  print_endline (State.name active_p ^ " landed on " ^ name board curr_pos ^ ":");
  print_endline (description board curr_pos);
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("Your current balance is "
    ^ string_of_int (State.current_balance active_p)
    ^ ".");
  print_endline " ";
  match space_type board curr_pos with
  | str ->
      (*each one of these helper functions must update game end and active
        player*)
      if str = "property" then ()
      else if str = "go" then ()
      else if str = "jail" then ()
      else if str = "chance" then ()
      else if str = "comm-chest" then ()

(*Suppose to handle the turn switching of players*)
and run_multi board arr chance_deck comm_deck bank =
  while !game_end != true do
    multi_player_run board arr chance_deck comm_deck bank !active_player
  done

(*takes in lst of names and creates array of players and initialises the game
  board *)
let initialise lst =
  let board_json = Board.from_json (Yojson.Basic.from_file "data/board.json") in
  let chance_deck =
    Deck.from_json (Yojson.Basic.from_file "data/chance.json")
  in
  let comm_deck =
    Deck.from_json (Yojson.Basic.from_file "data/comm-chest.json")
  in
  let name_arr = Array.of_list lst in
  let bank = Bank.init_bank 1000000 in
  (*for loop to initialise game state*)
  (* TODO: CHANGE NAME AT THE TIME OF PLAYER INITIALIZATION*)
  let arr = Array.make !num_player (init_state "") in
  for i = 0 to !num_player - 1 do
    arr.(i) <- init_state name_arr.(i)
  done;
  run_multi board_json arr chance_deck comm_deck bank

(*init_array takes in the number of players and creates an array to keep track
  of the players. It also passes the array and prompts for names of the
  players *)
let rec init_array str =
  (*try*)
  let x = int_of_string str in
  num_player := x;
  (*let arr = Array.make x None in*)
  let lst = get_name () in
  initialise lst
(*with Failure y -> ( print_endline "Oops! thats not a valid number: Make
  sure\n\ \ youare using integers like 1,2,3...and so on "; print_string "> ";
  match read_line () with | str -> init_array str | exception End_of_file ->
  ())*)

(*[main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Cornopoly!\n";
  print_endline
    "Please enter the number of players you are playing cornopoly with: ";
  print_string "> ";
  match read_line () with
  | str -> init_array str
  | exception End_of_file -> ()

(* Execute the game engine. *)
let () = main ()

(*NOTES/COMMENTS/CONCERNS*)
