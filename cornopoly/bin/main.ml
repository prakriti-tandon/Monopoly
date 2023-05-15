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
  ANSITerminal.print_string [ ANSITerminal.red ] ("\n" ^ str ^ " has lost!\n");
  exit 0

let quit_game () =
  ANSITerminal.print_string [ ANSITerminal.green ] "Thanks for playing!\n";
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
   with State.InsufficientFunds ->
     print_endline
       "\n\
        You didn't have enough money to buy that property... you went bankrupt!";
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
        print_string "> ";
        self_own_comm ()
  in
  print_string "The price to buy this property is ";
  ANSITerminal.print_string [ ANSITerminal.green ]
    (string_of_int (Board.price board space) ^ ".\n");
  print_endline "Buy property? [ type \"yes\" or \"no\"  ]";
  print_string "> ";
  self_own_comm

let buy_addon active_p space board bank arr buy_func str_desc =
  (try
     let new_pl = buy_func active_p space board 1 bank in
     Property.update_player arr active_p new_pl
   with
  | State.InsufficientFunds ->
      print_endline
        ("\nYou didn't have enough money to buy that " ^ str_desc
       ^ "... you went bankrupt!");
      terminate (State.name active_p)
  | ExceededHouseLimit ->
      print_endline
        ("Cannot complete transaction, too many " ^ str_desc
       ^ "s on this property."));
  update_active arr (Property.find_index (State.name active_p) arr)

let buy_hotel active_p space board bank arr =
  buy_addon active_p space board bank arr State.buy_hotel "hotel"

let buy_house active_p space board bank arr =
  buy_addon active_p space board bank arr State.buy_house "house"

let prompt_buy_addon space board active_p bank arr buy_fn price_fn str_desc =
  let rec self_own_comm () =
    match get_command_2 () with
    | Yes -> buy_fn active_p space board bank arr
    | No -> update_active arr (Property.find_index (State.name active_p) arr)
    | Quit -> quit_game ()
    | _ ->
        print_endline
          ("Invalid command. Type \"yes\" to buy a " ^ str_desc ^ " or \"no\".");
        print_string "> ";
        self_own_comm ()
  in
  print_string ("The price to build a " ^ str_desc ^ " is ");
  ANSITerminal.print_string [ ANSITerminal.green ]
    (string_of_int (price_fn board space) ^ ".\n");
  print_endline ("Build a " ^ str_desc ^ "? [ type \"yes\" or \"no\"  ]");
  print_string "> ";
  self_own_comm

let prompt_buy_hotel space board active_p bank arr =
  prompt_buy_addon space board active_p bank arr buy_hotel Board.price_per_hotel
    "hotel"

let prompt_buy_house space board active_p bank arr =
  prompt_buy_addon space board active_p bank arr buy_house Board.price_per_house
    "house"

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

let find_other_player (pls : player_list) (curr_pl : State.t) (board : Board.t)
    =
  match Property.property_status pls curr_pl board with
  | OwnedByOtherPlayer y -> y
  | _ -> failwith "impossible"

let pay_rent active_p space_num board bank arr =
  print_endline "This property is owned by another player.";
  print_string "You must pay a rent of ";
  ANSITerminal.print_string [ ANSITerminal.green ]
    (string_of_int
       (Property.determine_rent
          (find_other_player arr active_p board)
          space_num board)
    ^ ".\n");
  (try Property.pay_rent arr active_p board
   with Property.InsufficientFunds ->
     print_endline "You went bankrupt!";
     terminate (State.name active_p));
  update_active arr (Property.find_index (State.name active_p) arr)

let deal_property board arr int space_num bank =
  let active_p = arr.(int) in
  match property_status arr active_p board with
  | OwnedByOtherPlayer st -> pay_rent active_p space_num board bank arr
  | NotOwned -> ask_buy active_p space_num board bank arr ()
  | OwnedByThisPlayer -> self_own_prompt space_num board active_p bank arr ()

(* Operations within multi_player_run handle the paying of salary, so when the
   player lands on the Go space, we just print a friendly message saying that
   they don't need to do anything and update who the active player is.*)
let deal_go board arr int =
  print_endline "No need to do anything here!";
  update_active arr int

let turns_in_jail_to_string (player : State.t) : string =
  match State.jail player with
  | Some x ->
      let num = string_of_int x in
      if x + 1 >= 2 then num ^ " turns left." else num ^ " turn left."
  | None ->
      "0 turns left. This is your last day in jail! \n\
      \ Your mom is coming to pick you up tomorrow from the station. Get ready \
       to face her wrath."

(*TODO*)
let deal_jail board arr int =
  Property.update_player arr arr.(int) (put_in_jail arr.(int));
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You have " ^ turns_in_jail_to_string arr.(int) ^ "\n");
  print_endline "You are just visiting, nothing to worry about!";

  update_active arr int

(* This function chooses a chance card for the player, executes the chance card,
   then updates who the active player is. This NEEDS TO BE UPDATED to handle the
   case where a card bankrupts a player. *)
let deal_card board arr deck bank exec_fn int =
  let do_card board arr deck bank exec_fn int =
    let active_p = arr.(int) in
    let chosen_card = Deck.random_card deck () in
    print_endline ("You drew " ^ Deck.name deck chosen_card ^ "!");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (Deck.description deck chosen_card ^ "\n");
    exec_fn deck arr bank board chosen_card active_p;
    if current_balance arr.(int) <= 0 then (
      print_endline "You went bankrupt!";
      terminate (State.name arr.(int)))
    else ();
    update_active arr int
  in
  let rec draw_card board arr deck bank exec_fn int =
    match get_command_2 () with
    | Draw -> (
        print_endline "";
        try do_card board arr deck bank exec_fn int
        with State.InsufficientFunds ->
          terminate (State.name arr.(!active_player)))
    | Quit -> quit_game ()
    | _ ->
        print_endline "Oops, type \"draw\" to draw a card!";
        print_string "> ";
        draw_card board arr deck bank exec_fn int
  in
  print_endline "Type \"draw\" to draw a card!";
  print_string "> ";
  draw_card board arr deck bank exec_fn int;
  print_endline ""

let deal_chance board arr chance_deck bank int =
  deal_card board arr chance_deck bank Chance.exec_card int

let deal_commchest board arr comm_deck bank int =
  deal_card board arr comm_deck bank Comm_chest.exec_card int

let init_loan arr bank curr_player =
  Bank.deduct_funds bank 100;
  let new_p = State.change_owes curr_player 100 in
  let new_p2 = State.change_balance new_p 100 in
  Property.update_player arr curr_player new_p2;
  print_endline
    "\nYou now owe the bank $100, and $100 has been added to your balance.";
  print_endline "You can pass Go twice before you must fully repay your loan."

let offer_loan arr bank curr_player =
  print_endline "";
  print_endline
    "Your balance is running low... would you like to take out a loan for $100?";
  print_endline "Enter \"yes\" or \"no\".";
  print_string "> ";
  let rec loan_comm arr bank curr_player =
    match get_command_2 () with
    | Quit -> quit_game ()
    | Yes -> init_loan arr bank curr_player
    | No -> ()
    | _ ->
        print_endline
          "Invalid command. Type \"yes\" to take out a loan, or \"no\"";
        print_string "> ";
        loan_comm arr bank curr_player
  in
  loan_comm arr bank curr_player

let reset_owes () = failwith "unimplemented - reset owes"

let loan_transaction arr bank active_player i =
  Bank.add_funds bank i;
  let new_p = State.change_owes active_player ~-i in
  Property.update_player arr active_player new_p;
  match owes_to_bank new_p with
  | Some 0, _ -> reset_owes ()
  | Some x, _ -> ()
  | None, _ -> ()

let repay_loan arr bank active_p owes =
  let curr_bal = State.current_balance active_p in
  print_endline "How much would you like to pay?";
  print_string "> ";
  let rec repay_loan_comm arr bank curr_player curr_bal =
    match get_command_2 () with
    | Quit -> quit_game ()
    | Number_of_players i ->
        if i > curr_bal then (
          print_endline
            ("You do not have enough balance to pay $" ^ string_of_int i ^ ".");
          print_endline "Maybe try a lower number?";
          print_string "> ";
          repay_loan_comm arr bank curr_player curr_bal)
        else loan_transaction arr bank curr_player i
    | _ ->
        print_endline
          "Invalid command. Type \"yes\" to repay some of your loan, or \"no\"";
        print_string "> ";
        repay_loan_comm arr bank curr_player curr_bal
  in
  repay_loan_comm arr bank active_p curr_bal;
  print_endline ""

let prompt_repay_loan arr bank active_p owes go_left =
  let rec repay_loan_yncomm arr bank curr_player =
    match get_command_2 () with
    | Quit -> quit_game ()
    | Yes -> repay_loan arr bank curr_player
    | No -> fun () -> ()
    | _ ->
        print_endline
          "Invalid command. Type \"yes\" to repay some of your loan, or \"no\"";
        print_string "> ";
        repay_loan_yncomm arr bank curr_player
  in
  print_endline "";
  print_endline "Would you like to repay some of your loan?";
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\nYou may pass Go " ^ string_of_int go_left
   ^ " more times before it must be paid.\n");
  print_endline
    ("You owe " ^ string_of_int owes ^ " and your balance is "
    ^ string_of_int (current_balance active_p)
    ^ ".");
  print_endline "Type \"yes\" to repay some of your loan, or \"no\".";
  print_string "> ";
  repay_loan_yncomm arr bank active_p ();
  print_endline ""

let rec multi_player_run board arr chance_deck comm_deck bank int =
  let player_turn board arr chance_deck comm_deck bank int =
    let active_p = arr.(int) in
    (* Check if player has a loan before they potentially pass Go! *)
    (match owes_to_bank active_p with
    | Some x, y ->
        if y = 0 then (
          print_endline "You failed to repay your loan!";
          terminate (State.name active_p))
        else prompt_repay_loan arr bank active_p x y
    | None, _ -> ());
    let curr_bal = State.current_balance active_p in
    if curr_bal < 50 && fst (owes_to_bank active_p) = None then
      offer_loan arr bank active_p
    else ();
    (* refind active player because balance may have changed due to loan! *)
    let active_p = arr.(int) in
    let n = roll_die () in
    let old_pos = current_pos active_p in
    (*active_p_new is the updated state on moving the active player*)
    let active_p_new = State.go n active_p board in
    (*updating the state in the array*)
    arr.(int) <- active_p_new;
    print_endline "";
    print_endline ("You rolled a " ^ string_of_int n ^ "!");
    (*check if current spot is a property*)
    let curr_pos = current_pos active_p_new in
    (* Real quick, if player passed Go as they moved to this spot, pay them a
       salary. *)
    if old_pos > curr_pos then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "You passed Go. Enjoy your salary!\n";
      exec_go arr arr.(int) board)
    else ();
    (*print description and name of the property and check for owner *)
    print_endline
      (State.name active_p ^ " landed on " ^ Board.name board curr_pos ^ ":");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (Board.description board curr_pos ^ "\n");
    let curr_bal = State.current_balance arr.(int) in
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("Your current balance is " ^ string_of_int curr_bal ^ ".");
    print_endline " ";
    (match space_type board curr_pos with
    | str ->
        (*each one of these helper functions must update game end and active
          player*)
        if str = "property" then deal_property board arr int curr_pos bank
        else if str = "go" then deal_go board arr int
        else if str = "jail" then deal_jail board arr int
        else if str = "chance" then deal_chance board arr chance_deck bank int
        else if str = "comm-chest" then
          deal_commchest board arr comm_deck bank int);
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("Your balance is now "
      ^ string_of_int (State.current_balance arr.(int))
      ^ ".");
    print_endline ""
  in
  let rec start_turn_comm board arr chance_deck comm_deck bank int =
    match get_command_2 () with
    | Quit -> quit_game ()
    | Go -> (
        match jail arr.(int) with
        | None -> player_turn board arr chance_deck comm_deck bank int
        | Some x ->
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You're in jail, so you can't take a turn :( \n" ^ "You have "
              ^ turns_in_jail_to_string (turn_in_jail arr.(int) 1)
              ^ " \n");
            ANSITerminal.print_string [ ANSITerminal.red ]
              ("Your balance is now "
              ^ string_of_int (State.current_balance arr.(int))
              ^ ".");
            print_endline "";
            Property.update_player arr arr.(int) (turn_in_jail arr.(int) 1);
            update_active arr int)
    | _ ->
        print_endline
          "Invalid command. Type \"go\" to start your turn, or \"quit\" to \
           quit the game.";
        print_string "> ";
        start_turn_comm board arr chance_deck comm_deck bank int
  in
  let active_p = arr.(int) in
  print_string "\n";
  print_endline
    ("It is player " ^ State.name active_p
   ^ "'s turn. Type \"go\" to start your turn, or \"quit\" to quit.");
  print_string "> ";
  start_turn_comm board arr chance_deck comm_deck bank int

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
      \  you are using integers like 1,2,3...and so on ";
    print_string "> ";
    match read_line () with
    | str -> init_array str
    | exception End_of_file -> ())

(*[main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Cornopoly!\n";
  print_endline
    "Please enter the number of players you are playing Cornopoly with: ";
  print_string "> ";
  match read_line () with
  | str -> init_array str
  | exception End_of_file -> ()

(* Execute the game engine. *)
let () = main ()

(*NOTES/COMMENTS/CONCERNS*)
