type property = {
  space : int;
  num_houses : int;
  num_hotels : int;
}

type t = {
  name : string;
  current_pos : int;
  money : int;
  owns : property list;
  owes_to_bank : int option * int;
  jail : int option;
}

exception InsufficientFunds
exception ExceededHouseLimit
exception ExceededHotelLimit
exception DoesntOwnProperty

let init_state str =
  {
    name = str;
    current_pos = 0;
    money = 500;
    owns = [];
    owes_to_bank = (None, 0);
    jail = None;
  }

let name (player : t) = player.name
let current_pos (player : t) = player.current_pos
let owns_list (player : t) = player.owns

let make_property (space : int) (num_houses : int) (num_hotels : int) =
  { space; num_houses; num_hotels }

let num_houses (player : t) (space : int) =
  let rec num owns_list space =
    match player.owns with
    | [] -> raise DoesntOwnProperty
    | h :: t -> if h.space = space then h.num_houses else num t space
  in
  num player.owns space

let num_hotels (player : t) (space : int) =
  let rec num owns_list space =
    match player.owns with
    | [] -> raise DoesntOwnProperty
    | h :: t -> if h.space = space then h.num_hotels else num t space
  in
  num player.owns space

let owes_to_bank (player : t) = player.owes_to_bank

let change_owes (player : t) (amt : int) =
  let new_owes current_owes_to_bank =
    match current_owes_to_bank with
    | Some x ->
        let new_owes_to_bank = x + amt in
        (Some new_owes_to_bank, snd player.owes_to_bank)
    | None ->
        let new_owes_to_bank = amt in
        (Some new_owes_to_bank, 2)
  in
  {
    name = player.name;
    current_pos = player.current_pos;
    money = player.money;
    owns = player.owns;
    owes_to_bank = new_owes (fst player.owes_to_bank);
    jail = player.jail;
  }

let jail (player : t) = failwith "unimplemented"
let put_in_jail (player : t) = failwith "unimplemented"
let get_out_of_jail (player : t) = failwith "unimplemented"
let current_balance (player : t) = player.money

let change_balance (player : t) (amt : int) =
  let new_amt = player.money + amt in
  {
    name = player.name;
    current_pos = player.current_pos;
    money = new_amt;
    owns = player.owns;
    owes_to_bank = player.owes_to_bank;
    jail = player.jail;
  }

let rec owns (player : t) (space : int) (game : Board.t) =
  match player.owns with
  | [] -> false
  | h :: t ->
      if h.space = space then true else owns { player with owns = t } space game

let compare_property (property1 : property) (property2 : property) : int =
  let diff = property1.space - property2.space in
  if diff > 0 then 1 else if diff = 0 then 0 else -1

let change_owns pos play1 =
  {
    play1 with
    owns =
      List.sort_uniq compare_property
        ({ space = pos; num_houses = 0; num_hotels = 0 } :: play1.owns);
  }

let dice = Random.int 7

let go (dice : int) (player : t) (game : Board.t) =
  let dice_result = dice in
  let result_position =
    (player.current_pos + dice_result) mod Board.number_of_spaces game
  in
  {
    name = player.name;
    current_pos = result_position;
    money = player.money;
    owns = player.owns;
    owes_to_bank = player.owes_to_bank;
    jail = player.jail;
  }

let buy_property (player1 : t) (space : int) (game : Board.t) (bank : Bank.t) =
  failwith "Unimplemented"
(*let new_funds = player1.money - Board.price game space in if new_funds < 0
  then raise InsufficientFunds else let new_owns = player1.owns @ [ space ] in

  { name = player1.name; current_pos = player1.current_pos; money = new_funds;
  owns = new_owns; }*)

let buy_house (player1 : t) (space : int) (game : Board.t) (num_houses : int)
    (bank : Bank.t) : t =
  failwith "unimplemented"

(* Lauren here - i need this function for one of the chance cards called
   "hotelie" where i give away a hotel for free! I added it to the interface so
   it can be visible on my end, but hopefully it will be a useful helper for you
   when implemented buy hotel as well.*)
let add_hotel (player : t) (space : int) (game : Board.t) =
  failwith "Unimplemented"

let buy_hotel (player1 : t) (space : int) (game : Board.t) (num_hotels : int)
    (bank : Bank.t) =
  failwith "unimplemented"

let sell_property (player : t) (space : int) (game : Board.t) =
  failwith "unimplemented"

let sell_house (player : t) (space : int) (game : Board.t) =
  failwith "unimplemented"

let sell_hotel (player : t) (space : int) (game : Board.t) =
  failwith "unimplemented"

let space_of_property p = p.space
let remove_owns (space : int) (player : t) = failwith "Unimplemented"
