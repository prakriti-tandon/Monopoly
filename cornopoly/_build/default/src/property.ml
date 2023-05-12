type status =
  | OwnedByOtherPlayer of State.t
  | NotOwned
  | OwnedByThisPlayer

type player_list = State.t array

exception InsufficientFunds
exception SpaceNotOwnable

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
  let base_rent =
    try Board.rent board property
    with Board.SpaceNotOwnable -> raise SpaceNotOwnable
  in
  let num_houses = State.num_houses owner property in
  let num_hotels = State.num_hotels owner property in
  let rent_per_hotel = Board.rent_per_hotel board property in
  let rent_per_house = Board.rent_per_house board property in
  match (num_houses, num_hotels) with
  | 0, 0 -> base_rent
  | 0, 1 -> base_rent + rent_per_hotel
  | 0, 2 -> base_rent + (2 * rent_per_hotel)
  | 1, 0 -> base_rent + rent_per_house
  | 1, 1 -> base_rent + rent_per_hotel + rent_per_house
  | 1, 2 -> base_rent + (2 * rent_per_hotel) + rent_per_house
  | 2, 0 -> base_rent + (2 * rent_per_house)
  | 2, 1 -> base_rent + rent_per_hotel + (2 * rent_per_house)
  | 2, 2 -> base_rent + (2 * rent_per_hotel) + (2 * rent_per_house)
  | 3, 0 -> base_rent + (3 * rent_per_house)
  | 3, 1 -> base_rent + rent_per_hotel + (3 * rent_per_house)
  | 3, 2 -> base_rent + (2 * rent_per_hotel) + (3 * rent_per_house)
  | 4, 0 -> base_rent + (4 * rent_per_house)
  | 4, 1 -> base_rent + rent_per_hotel + (4 * rent_per_house)
  | 4, 2 -> base_rent + (2 * rent_per_hotel) + (4 * rent_per_house)
  | _ -> failwith "impossible"

let determine_price (owner : State.t) (property : int) (board : Board.t) =
  let base_price =
    try Board.price board property
    with Board.SpaceNotOwnable -> raise SpaceNotOwnable
  in
  let num_houses = State.num_houses owner property in
  let num_hotels = State.num_hotels owner property in
  let price_per_hotel = Board.price_per_hotel board property in
  let price_per_house = Board.price_per_house board property in
  match (num_houses, num_hotels) with
  | 0, 0 -> base_price
  | 0, 1 -> base_price + price_per_hotel
  | 0, 2 -> base_price + (2 * price_per_hotel)
  | 1, 0 -> base_price + price_per_house
  | 1, 1 -> base_price + price_per_hotel + price_per_house
  | 1, 2 -> base_price + (2 * price_per_hotel) + price_per_house
  | 2, 0 -> base_price + (2 * price_per_house)
  | 2, 1 -> base_price + price_per_hotel + (2 * price_per_house)
  | 2, 2 -> base_price + (2 * price_per_hotel) + (2 * price_per_house)
  | 3, 0 -> base_price + (3 * price_per_house)
  | 3, 1 -> base_price + price_per_hotel + (3 * price_per_house)
  | 3, 2 -> base_price + (2 * price_per_hotel) + (3 * price_per_house)
  | 4, 0 -> base_price + (4 * price_per_house)
  | 4, 1 -> base_price + price_per_hotel + (4 * price_per_house)
  | 4, 2 -> base_price + (2 * price_per_hotel) + (4 * price_per_house)
  | _ -> failwith "impossible"

let find_index (name : string) (pls : player_list) : int =
  let res = ref 0 in
  let size = Array.length pls in
  for x = 0 to size - 1 do
    let player = pls.(x) in
    if State.name player = name then res := x else ()
  done;
  !res

let pay_rent (pls : player_list) (curr_pl : State.t) (board : Board.t) : unit =
  match property_status pls curr_pl board with
  | NotOwned -> ()
  | OwnedByThisPlayer -> ()
  | OwnedByOtherPlayer owner ->
      let rent = determine_rent owner (State.current_pos curr_pl) board in
      if rent > State.current_balance curr_pl then raise InsufficientFunds
      else
        let new_pl = State.change_balance curr_pl (-rent) in
        let new_owner = State.change_balance owner rent in
        let curr_pl_index = find_index (State.name curr_pl) pls in
        let owner_index = find_index (State.name owner) pls in
        pls.(curr_pl_index) <- new_pl;
        pls.(owner_index) <- new_owner

let new_player property price n_houses n_hotels curr_pl board =
  let temp =
    State.change_balance curr_pl (-price) |> State.change_owns property
  in
  let price_adjust =
    State.change_balance temp
      ((n_houses * Board.price_per_house board property)
      + (n_hotels * Board.price_per_hotel board property))
  in
  let house_state =
    State.buy_house price_adjust property board n_houses (Bank.init_bank 0)
  in
  State.buy_hotel house_state property board n_hotels (Bank.init_bank 0)

let buy_property_from_player (pls : player_list) (curr_pl : State.t)
    (board : Board.t) =
  match property_status pls curr_pl board with
  | NotOwned -> ()
  | OwnedByThisPlayer -> ()
  | OwnedByOtherPlayer owner ->
      let property = State.current_pos curr_pl in
      let price = determine_price owner property board in
      let n_houses = State.num_houses owner property in
      let n_hotels = State.num_hotels owner property in
      if price > State.current_balance curr_pl then raise InsufficientFunds
      else
        let new_pl =
          new_player property price n_houses n_hotels curr_pl board
        in

        let new_owner =
          State.change_balance owner
            price (*change owns, number of houses, hotels*)
        in
        let curr_pl_index = find_index (State.name curr_pl) pls in
        let owner_index = find_index (State.name owner) pls in
        pls.(curr_pl_index) <- new_pl;
        pls.(owner_index) <- new_owner
