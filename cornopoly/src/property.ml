type status =
  | OwnedByOtherPlayer of State.t
  | NotOwned
  | OwnedByThisPlayer

type player_list = State.t array

exception InsufficientFunds

let property_status (pls : player_list) (curr_pl : State.t) (board : Board.t) :
    status =
  let curr_pos = State.current_pos curr_pl in
  let check_owns (player : State.t) = State.owns player curr_pos board in
  match Array.find_opt check_owns pls with
  | None -> NotOwned
  | Some player ->
      if State.name player = State.name curr_pl then OwnedByThisPlayer
      else OwnedByOtherPlayer player

let determine_rent (owner : State.t) (property : int) (board : Board.t) : int =
  failwith "Unimplemented"

let determine_price (owner : State.t) (property : int) (board : Board.t) =
  failwith "Unimplemented"

let pay_rent (pls : player_list) (curr_pl : State.t) (board : Board.t) =
  failwith "Unimplemented"

let buy_property_from_player (pls : player_list) (curr_pl : State.t)
    (board : Board.t) =
  failwith "Unimplemented"
