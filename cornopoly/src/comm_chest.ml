(* community chest code here *)

let current_player (pls : Property.player_list) : State.t =
  failwith "unimplemented - current player [comm chest]"

let update_pls_newamt pls player amt =
  Property.update_player pls player (State.change_balance player amt)

(* TODO: do y'all want me to be messing with the bank? if yes, we need to update
   the bank .mli *)
let exec_earn pls bank x curr_player =
  Bank.deduct_funds bank x;
  update_pls_newamt pls curr_player x

let exec_pay pls bank x curr_player =
  Bank.add_funds bank x;
  update_pls_newamt pls curr_player ~-x

let exec_happy_birthday pls curr_player =
  for i = 0 to Array.length pls do
    if pls.(i) = curr_player then
      update_pls_newamt pls curr_player (10 * (Array.length pls - 1))
    else update_pls_newamt pls pls.(i) ~-10
  done

let exec_boba pls curr_player =
  for i = 0 to Array.length pls do
    if pls.(i) = curr_player then
      update_pls_newamt pls curr_player ~-(10 * (Array.length pls - 1))
    else update_pls_newamt pls pls.(i) 10
  done

let exec_slopeday pls curr_player =
  for i = 0 to Array.length pls - 1 do
    update_pls_newamt pls pls.(i) ~-90
  done

let find_richest (pls : Property.player_list) =
  let x = ref pls.(0) in
  for i = 0 to Array.length pls do
    if State.current_balance pls.(i) > State.current_balance !x then
      x := pls.(i)
  done;
  !x

let find_second_richest (pls : Property.player_list) =
  let richest = find_richest pls in
  let x = if pls.(0) = richest then ref pls.(1) else ref pls.(0) in
  for i = 0 to Array.length pls do
    if
      State.current_balance pls.(i) > State.current_balance !x
      && pls.(i) <> richest
    then x := pls.(i)
  done;
  !x

let exec_first_apt pls curr_player =
  let richest = find_richest pls in
  if richest = curr_player then update_pls_newamt pls curr_player ~-30
  else update_pls_newamt pls curr_player ~-15;
  update_pls_newamt pls richest ~-15

let exec_fortune pls curr_player =
  let amt =
    let _ = Random.self_init () in
    fun () -> Random.int 100
  in
  update_pls_newamt pls curr_player (amt ())

let exec_socialist pls =
  let richest = find_richest pls in
  let second_richest = find_second_richest pls in
  let excess =
    State.current_balance richest - State.current_balance second_richest
  in
  let num_other_players = Array.length pls - 1 in
  let redistribution = excess / num_other_players in
  for i = 0 to Array.length pls do
    if pls.(i) = richest then
      update_pls_newamt pls richest ~-(redistribution * num_other_players)
    else update_pls_newamt pls pls.(i) redistribution
  done

let exec_other pls bank i curr_player =
  match i with
  | 0 -> exec_happy_birthday pls curr_player
  | 5 -> exec_boba pls curr_player
  | 7 -> exec_slopeday pls curr_player
  | 10 -> exec_first_apt pls curr_player
  | 15 -> exec_fortune pls curr_player
  | 16 -> exec_socialist pls
  | _ -> failwith "Unimplemented - comm chest other card"

let exec_card deck pls bank board i curr_player =
  match Deck.comm_chest_info deck i with
  | Earn (Some x) -> exec_earn pls bank x curr_player
  | Pay (Some x) -> exec_pay pls bank x curr_player
  | Other -> exec_other pls bank i curr_player
  | _ -> failwith "impossible - comm. chest card with Earn/Pay None"
