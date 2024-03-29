(* chance code here *)

(* Executes cards that take player to a new space *)
let move_player pls board curr_player newspace =
  let dice_roll =
    Board.between_spaces board (State.current_pos curr_player) newspace
  in
  Multiplayer.update_player pls curr_player
    (State.go dice_roll curr_player board);
  if newspace = 15 then
    let newp = pls.(Multiplayer.find_index (State.name curr_player) pls) in
    Multiplayer.update_player pls newp (State.put_in_jail newp)
  else ()

let free_something (board : Board.t) (bank : Bank.t) curr_player pls prop func =
  let new_pstate = func curr_player (State.space_of_property prop) board 1 in
  Multiplayer.update_player pls curr_player new_pstate

let free_hotel board bank curr_player pls (prop : State.property) =
  if State.num_hotels curr_player (State.space_of_property prop) < 2 then
    free_something board bank curr_player pls prop State.add_hotel
  else ()

let free_house board bank curr_player pls (prop : State.property) =
  if State.num_houses curr_player (State.space_of_property prop) < 4 then
    free_something board bank curr_player pls prop State.add_house
  else ()

let cheapest_property pls bank board curr_player func =
  match List.sort State.compare_property (State.owns_list curr_player) with
  | [] -> ()
  | [ prop ] -> prop |> func board bank curr_player pls
  | h :: t -> h |> func board bank curr_player pls

(* Executes new house card (old name)*)
let exec_sell_prop pls bank board curr_player =
  cheapest_property pls bank board curr_player free_house

(* Executes hotelie card *)
let exec_hotelie pls bank board curr_player =
  cheapest_property pls bank board curr_player free_hotel

(* Executes card that takes you to next chance space *)
let exec_double_chance pls board curr_player =
  if State.current_pos curr_player = 7 then move_player pls board curr_player 23
  else move_player pls board curr_player 7

(* Executes card that takes you to the next comm. chest. space *)
let exec_comm_chest pls board curr_player =
  if State.current_pos curr_player = 7 then move_player pls board curr_player 11
  else move_player pls board curr_player 27

(* Executes nonstandard cards *)
let exec_other pls bank board i curr_player =
  match i with
  | 6 -> exec_sell_prop pls bank board curr_player
  | 11 -> exec_hotelie pls bank board curr_player
  | 13 -> exec_double_chance pls board curr_player
  | 14 -> exec_comm_chest pls board curr_player
  | _ -> failwith "unimplemented - misc. chance card"

let exec_card deck pls bank board i curr_player =
  match Deck.new_space deck i with
  | Some newspace -> move_player pls board curr_player newspace
  | None -> exec_other pls bank board i curr_player
