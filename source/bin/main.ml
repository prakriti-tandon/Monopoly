(*Open statements*)
open Game
open Command
open State
open Monopoly

(*pay rent illegal -> termination*)
(*CHANGE: Needs to take in the name of the player that wins and show that as the game gets over *)
let terminate() = 
print_endline "Game over";
exit 0

let rec roll_die() = 
  let n = Random.int (7) in 
  if n!=0 then n else roll_die()


  let trans st1 st2 board = raise (Failure "unimp")
  let prompt_buy st1 st2 board =raise (Failure "unimp")

(*runs through one player turn which involves changing states, paying rent etc*)
let player_run st1 st2 board= 
(*printing the name of the player whose turn it is currently*)
print_endline ("It is player "^ State.name st1^"'s turn");
print_endline (">");
(*move player to new block*)
let st_go = State.go st1 board in 
let curr_pos = current_pos st_go in 
(*check if block is owned or not -> if owned then pay rent otherwise prompt buying*)
match owner board curr_pos with 
| Some s -> let lst = trans st1 st2 in 
| None -> prompt_buy st1 st2 board


(*main game loop*)
let rec game_loop st1 st2 board= 
  (*check termination state which is only when either of the players have negative amount of money*)
  if current_balance st1 <= 0 || current_balance st2 <= 0 then terminate()
  else
    let st1_new = player_run st1 st2 board in
    let st2_new = player_run st2 st1 board in game_loop st1_new st2_new board

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

(*NOTES/CONCERS*)
(*Prak -> random int 7*)