(*Open statements*)
open Game
open Command
open State
open Board

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
let trans st1 st2 board = failwith "Commented out"
(*try let comb_state = State.pay_rent st1 st2 board in (comb_state.player1,
  comb_state.player2) with InsufficientFunds -> terminate (State.name st2)*)

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
    let st_new =
      State.buy_property st1 (current_pos st1) board (Bank.init_bank 5000)
    in
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
          | "y" -> prompt_buy st1_go st2 board
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

(*let st1_new = player_run st1 st2 board in let st2_new = player_run st2 st1
  board in game_loop st1_new st2_new board*)

(*Gets the list of names and initialises the game and then calls start game*)
let rec initialise_game lst =
  let board_json = Yojson.Basic.from_file "data/board.json" in
  match lst with
  | x :: y :: t ->
      let st1 = init_state x in
      let st2 = init_state y in
      game_loop st1 st2 (from_json board_json)
  | _ -> (
      print_endline
        "Not enough names to start the game. Make sure the first player's name \
         is followed by a single space followed by the second players name.";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | str -> get_name str)

(*main() and initialise_game helper which parses through the string given by the
  player and initialises the game *)
and get_name str =
  let lst = String.split_on_char ' ' str in
  initialise_game lst

(*[main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Cornopoly!\n";
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
(*deal with the situation when player 1 owns it  *)
