(*Open statements*)
open Game
open Command
open State
open Monopoly

(*ends the game and shows the name of the player that won*)
let terminate str = 
print_endline (str ^ " has won!");
exit 0

let rec roll_die() = 
  let n = Random.int (7) in 
  if n!=0 then n else roll_die()

(*both helpers need to return a tupple of s1 and st2 where the first *)
  let trans st1 st2 board =     
    try let comb_state = State.pay_rent (st1) (st2) (board) in (comb_state.player1, comb_state.player2)
    with
|   InsufficientFunds -> terminate (State.name st2)

  (*recursive function that loops through and annoys the user till they return either y or n as a value*)
  let rec get_command() =  
  match read_line() with 
  | "y" -> "y"
  | "n" -> "n"
  | _ -> 
    print_endline ("Oh no thats not a valid command type y for yes and n for no"); 
    print_endline("> ");
    get_command() 
  
  let prompt_buy st1 st2 board = 
    try let st_new = State.buy_property (st1) (current_pos st1) (board) in (st_new,st2)
  with
  | InsufficientFunds -> terminate (State.name st2)

(*runs through one player turn which involves changing states, paying rent etc*)
let player_run st1 st2 board= 
(*printing the name of the player whose turn it is currently*)
print_endline ("It is player "^ State.name st1^"'s turn");
(*throw die to get a number and then move player 1 that many blocks*)
let st1_go = State.go (roll_die())(st1) board in
(*check if current spot is a property*)
let curr_pos = current_pos st1_go in 
(*print description of the property and check for owner *)
print_endline (State.name st1^ " landed on "^ description board curr_pos );
match (State.owns st2 curr_pos board) with 
| true -> 
  (*here the first argument is the player who is paying and second arg is the one who is receiving*)
  trans st1_go st2 board
| false -> 
  print_endline ("Buy? [y/n]");
  print_endline (">");
  let str =  get_command() in
  begin
  match str with 
  | "y" -> prompt_buy st1_go st2 board 
  | "n" -> (st1_go, st2)
  | _ -> (st1_go, st2)
  end 

(*main game loop*)
let rec game_loop st1 st2 board= 
  (*note-to-self: termination is handled in the transaction functions not here -> hard to deal with the exceptions in the main loop*)
    (*the first argument put into player_run is the player whose turn it is right now*)
    let state_after1 = player_run st1 st2 board in 
    match state_after1 with 
    (*deals with player 2's turn and then recursively calls game_loop thereby starting player 1's turn again*)
    | (x,y) -> let state_after_2 = player_run y x board in game_loop (snd state_after_2) (fst state_after_2) board


    (*let st1_new = player_run st1 st2 board in
    let st2_new = player_run st2 st1 board in game_loop st1_new st2_new board*)

(*Gets the list of names and initialises the game and then calls start game*)
let rec initialise_game lst = 
  let board_json = Yojson.Basic.from_file ("data/board.json") in 
  match lst with 
  | x :: y :: t -> let st1 = 
      (init_state x) in let st2 = (init_state y) in
      (game_loop st1 st2 (from_json board_json))
  |_ -> 
    print_endline "Not enough names to start the game. Make sure the first player's name is followed by a single space followed by the second players name.";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | str -> get_name str

(*main() and initialise_game helper which parses through the string given by the
  player and initialises the game *)
and get_name str =
  let lst = String.split_on_char ' ' str in
  initialise_game lst

(*[main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nWelcome message to cornopoly\n";
  print_endline
    "Please enter the name of first player followed by space and then the name \
     of the second player";
  print_string "> ";
  match read_line () with
  | str -> get_name str
  | exception End_of_file -> ()

(* Execute the game engine. *)
let () = main ()

(*NOTES/COMMENTS/CONCERNS*)
(*Print enline new line what to do?*)