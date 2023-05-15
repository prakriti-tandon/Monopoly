(*Open statements*)
open Game
open Command
open State
open Board
open Property
open Go
open Deck

(*Global variable which is set at the time when we initialise the game*)
let num_player = ref 0
let game_end = ref false
let active_player = ref 0

(*ends the game and shows the name of the player that won*)
let terminate str =
  print_endline (str ^ " has lost!");
  exit 0

let quit_game () =
  print_endline "Thanks for playing!";
  exit 0

(*calls once and returns a function*)
let rec roll_die =
  let _ = Random.self_init () in
  fun () ->
    let n = Random.int 7 in
    if n != 0 then n else roll_die ()

(*both helpers need to return a tupple of s1 and st2 where the first *)

let rec get_command_2 () =
  try Command.parse (read_line ())
  with Empty ->
    print_endline "No input received, type something again!";
    print_string "> ";
    get_command_2 ()

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

(*main game loop let rec game_loop st1 st2 board = (*note-to-self: termination
  is handled in the transaction functions not here -> hard to deal with the
  exceptions in the main loop*) (*the first argument put into player_run is the
  player whose turn it is right now*) let state_after1 = player_run st1 st2
  board in match state_after1 with (*deals with player 2's turn and then
  recursively calls game_loop thereby starting player 1's turn again*) | x, y ->
  let state_after_2 = player_run y x board in game_loop (snd state_after_2) (fst
  state_after_2) board*)

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

let buy_property active_p space board bank arr =
  (try
     let new_pl = State.buy_property active_p space board bank in
     Property.update_player arr active_p new_pl
   with InsufficientFunds ->
     print_endline
       "You didn't have enough money to buy that property... you went bankrupt!";
     terminate (State.name active_p));
  update_active arr (Property.find_index (State.name active_p) arr)

let ask_buy active_p space board bank arr =
  let rec self_own_comm () =
    match get_command_2 () with
    | Yes -> buy_property active_p space board bank arr
    | No -> update_active arr (Property.find_index (State.name active_p) arr)
    | Quit -> quit_game ()
    | _ ->
        print_endline
          "Invalid command. Type \"yes\" to buy a property or \"no\".";
        self_own_comm ()
  in
  print_endline
    ("The price to buy this property is "
    ^ string_of_int (Board.price board space)
    ^ ".");
  print_endline "Buy property? [ type \"yes\" or \"no\"  ]";
  self_own_comm

let buy_house active_p space board bank arr =
  (try
     let new_pl = State.buy_house active_p space board 1 bank in
     Property.update_player arr active_p new_pl
   with
  | InsufficientFunds ->
      print_endline
        "You didn't have enough money to buy that house... you went bankrupt!";
      terminate (State.name active_p)
  | ExceededHouseLimit ->
      print_endline
        "Cannot complete transaction, too many houses on this property.");
  update_active arr (Property.find_index (State.name active_p) arr)

let buy_hotel active_p space board bank arr =
  (try
     let new_pl = State.buy_hotel active_p space board 1 bank in
     Property.update_player arr active_p new_pl
   with
  | InsufficientFunds ->
      print_endline
        "You didn't have enough money to buy that hotel... you went bankrupt!";
      terminate (State.name active_p)
  | ExceededHouseLimit ->
      print_endline
        "Cannot complete transaction, too many hotels on this property.");
  update_active arr (Property.find_index (State.name active_p) arr)

let prompt_buy_house space board active_p bank arr =
  let rec self_own_comm () =
    match get_command_2 () with
    | Yes -> buy_house active_p space board bank arr
    | No -> update_active arr (Property.find_index (State.name active_p) arr)
    | Quit -> quit_game ()
    | _ ->
        print_endline "Invalid command. Type \"yes\" to buy a house or \"no\".";
        self_own_comm ()
  in
  print_endline
    ("The price to build a house is "
    ^ string_of_int (Board.price_per_house board space)
    ^ ".");
  print_endline "Build a house? [ type \"yes\" or \"no\"  ]";
  self_own_comm

let prompt_buy_hotel space board active_p bank arr =
  let rec self_own_comm () =
    match get_command_2 () with
    | Yes -> buy_hotel active_p space board bank arr
    | No -> update_active arr (Property.find_index (State.name active_p) arr)
    | Quit -> quit_game ()
    | _ ->
        print_endline "Invalid command. Type \"yes\" to buy a hotel or \"no\".";
        self_own_comm ()
  in
  print_endline
    ("The price to build a hotel is "
    ^ string_of_int (Board.price_per_hotel board space)
    ^ ".");
  print_endline "Build a hotel? [ type \"yes\" or \"no\"  ]";
  self_own_comm

let self_own_prompt space board active_p bank arr =
  print_endline "You already own this property!";
  if num_houses active_p space < 5 then
    prompt_buy_house space board active_p bank arr
  else if num_hotels active_p space < 3 then
    prompt_buy_hotel space board active_p bank arr
  else (
    print_endline
      "You've maxed out on houses and hotels! That's very impressive.";
    fun () -> update_active arr (Property.find_index (State.name active_p) arr))
(*match read_line () with | str -> (*build a house if the player says yes
  otherwise next member's turn*) if str = "y" then buy_house () else
  active_player := (!active_player + 1) mod !num_player | exception End_of_file
  -> ()*)

let deal_property board arr int space_num bank =
  let active_p = arr.(int) in
  match property_status arr active_p board with
  | OwnedByOtherPlayer st -> ()
  | NotOwned -> ask_buy active_p space_num board bank arr ()
  | OwnedByThisPlayer -> self_own_prompt space_num board active_p bank arr ()

(* Operations within multi_player_run handle the paying of salary, so when the
   player lands on the Go space, we just print a friendly message saying that
   they don't need to do anything and update who the active player is.*)
let deal_go board arr int =
  print_endline "No need to do anything here!";
  update_active arr int

(*TODO*)
let deal_jail board arr int = raise (Failure "unimp")

(* This function chooses a chance card for the player, executes the chance card,
   then updates who the active player is. This NEEDS TO BE UPDATED to handle the
   case where a card bankrupts a player. *)
let deal_card board arr deck bank exec_fn int =
  let do_card =
    let active_p = arr.(int) in
    let chosen_card = Deck.random_card deck () in
    print_endline ("You drew " ^ Deck.name deck chosen_card ^ "!");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (Deck.description deck chosen_card);
    exec_fn deck arr bank board chosen_card active_p;
    update_active arr int
  in
  try do_card
  with InsufficientFunds -> terminate (State.name arr.(!active_player))

let deal_chance board arr chance_deck bank int =
  deal_card board arr chance_deck bank Chance.exec_card int

let deal_commchest board arr comm_deck bank int =
  deal_card board arr comm_deck bank Comm_chest.exec_card int

let rec multi_player_run board arr chance_deck comm_deck bank int =
  let active_p = arr.(int) in
  print_string "\n";
  print_endline ("It is player " ^ State.name active_p ^ "'s turn.");
  let n = roll_die () in
  let old_pos = current_pos active_p in
  (*active_p_new is the updated state on moving the active player*)
  let active_p_new = State.go n active_p board in
  (*updating the state in the array*)
  arr.(int) <- active_p_new;
  print_endline ("You rolled a " ^ string_of_int n ^ "!");
  (*check if current spot is a property*)
  let curr_pos = current_pos active_p_new in
  (* Real quick, if player passed Go as they moved to this spot, pay them a
     salary. *)
  if old_pos > curr_pos then (
    print_endline "You passed Go. Enjoy your salary!";
    exec_go arr arr.(int) board)
  else ();
  (*print description and name of the property and check for owner *)
  print_endline
    (State.name active_p ^ " landed on " ^ Board.name board curr_pos ^ ":");
  print_endline (Board.description board curr_pos);
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("Your current balance is "
    ^ string_of_int (State.current_balance active_p)
    ^ ".");
  print_endline " ";
  match space_type board curr_pos with
  | str ->
      (*each one of these helper functions must update game end and active
        player*)
      if str = "property" then deal_property board arr int curr_pos bank
      else if str = "go" then deal_go board arr int
      else if str = "jail" then deal_jail board arr int
      else if str = "chance" then deal_chance board arr chance_deck bank int
      else if str = "comm-chest" then
        deal_commchest board arr comm_deck bank int

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
  try
    let x = int_of_string str in
    num_player := x;
    (*let arr = Array.make x None in*)
    let lst = get_name () in
    initialise lst
  with Failure y -> (
    print_endline
      "Oops! thats not a valid number: Make\n\
      \  sure\n\
      \  youare using integers like 1,2,3...and so on ";
    print_string "> ";
    match read_line () with
    | str -> init_array str
    | exception End_of_file -> ())

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
