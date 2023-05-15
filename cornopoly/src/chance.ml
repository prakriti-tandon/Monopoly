(* chance code here *)

let move_player pls board curr_player newspace =
  let dice_roll =
    Board.between_spaces board (State.current_pos curr_player) newspace
  in
  Property.update_player pls curr_player (State.go dice_roll curr_player board)

let sell_prop board bank curr_player pls prop =
  let new_pstate =
    State.sell_property curr_player (State.space_of_property prop) board bank
  in
  Property.update_player pls curr_player new_pstate

let free_hotel board bank curr_player pls prop =
  let new_pstate =
    State.add_hotel curr_player (State.space_of_property prop) board 1
  in
  Property.update_player pls curr_player new_pstate

let cheapest_property pls bank board curr_player func =
  match List.sort State.compare_property (State.owns_list curr_player) with
  | [] -> ()
  | [ prop ] -> prop |> func board bank curr_player pls
  | h :: t -> h |> func board bank curr_player pls

let exec_sell_prop pls bank board curr_player =
  cheapest_property pls bank board curr_player sell_prop

let exec_hotelie pls bank board curr_player =
  cheapest_property pls bank board curr_player free_hotel

let exec_double_chance pls board curr_player =
  if State.current_pos curr_player = 7 then move_player pls board curr_player 23
  else move_player pls board curr_player 7

let exec_comm_chest pls board curr_player =
  if State.current_pos curr_player = 7 then move_player pls board curr_player 11
  else move_player pls board curr_player 27

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
