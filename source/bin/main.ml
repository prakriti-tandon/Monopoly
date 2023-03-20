(*Open statements*)
open Game
open Command
open State
open Monopoly

(*pay rent illegal -> termination*)
let terminate () = raise (Failure "unimplemented")

(*runs through one player turn which involves changing states, paying rent etc*)
let player_run st = raise (Failure "unimplemented")

(*Start game*)
let rec start_game st1 st2 =
  (*check termination state and then move forward*)
  let st1_new = player_run st1 in
  let st2_new = player_run st2 in
  start_game st1_new st2_new

(*Gets the list of names and initialises the game and then calls start game*)
let rec initialise_game lst =
  match lst with
  | x :: y :: t ->
      let st1 = init_state x in
      let st2 = init_state y in
      start_game st1 st2
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
