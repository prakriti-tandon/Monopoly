(*Open statements*)


(*pay rent illegal -> termination*)
let terminate() = raise (Failure "unimplemented")


(*runs through one player turn which involves changing states, paying rent etc*)
let player_run =raise (Failure "unimplemented")

(*Start game*)
let start_game st1 st2 = 
  let st_new = player_run st1 in 
  let st_new = player_run st2 in ()

(*Gets the list of names and initialises the game and then calls start game*)
let initialise_game lst = raise (Failure "unimplemented")

(*main() helper which parses through the string given by the player and returns a list of the players*)
let get_name str = raise (Failure "Unimplemented")

(*[main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nWelcome message to cornopoly\n";
  print_endline "Please enter the name of first player followed by space and then the name of the second player";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str ->get_name str

(* Execute the game engine. *)
let () = main ()

