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
    (* match player.owns with | [] -> raise DoesntOwnProperty | h :: t -> if
       h.space = space then h.num_houses else num t space *)
    match owns_list with
    | [] -> raise DoesntOwnProperty
    | h :: t -> if h.space = space then h.num_houses else num t space
  in
  num player.owns space

let num_hotels (player : t) (space : int) =
  let rec num owns_list space =
    (* match player.owns with | [] -> raise DoesntOwnProperty | h :: t -> if
       h.space = space then h.num_hotels else num t space *)
    match owns_list with
    | [] -> raise DoesntOwnProperty
    | h :: t -> if h.space = space then h.num_hotels else num t space
  in
  num player.owns space

let rec new_owns_list owns_list new_property acc =
  match owns_list with
  | [] -> acc
  | h :: t ->
      if h.space = new_property.space then
        new_owns_list t new_property (new_property :: acc)
      else h :: new_owns_list t new_property acc

(*this is basically a List.filter function which removes the old property and
  inserts the new version of the property*)
let add_house (player : t) (space : int) (game : Board.t) (x : int) =
  let rec num owns_list space =
    match owns_list with
    | [] -> raise DoesntOwnProperty
    | h :: t ->
        if h.space = space then
          let new_property =
            make_property space (h.num_houses + x) h.num_hotels
          in
          { player with owns = new_owns_list player.owns new_property [] }
        else num t space
  in
  num player.owns space

(* Lauren here - i need this function for one of the chance cards called
   "hotelie" where i give away a hotel for free! I added it to the interface so
   it can be visible on my end, but hopefully it will be a useful helper for you
   when implemented buy hotel as well.*)
let add_hotel (player : t) (space : int) (game : Board.t) (x : int) =
  let rec num owns_list space =
    match owns_list with
    | [] -> raise DoesntOwnProperty
    | h :: t ->
        if h.space = space then
          let new_property =
            make_property space h.num_houses (h.num_hotels + x)
          in
          { player with owns = new_owns_list player.owns new_property [] }
        else num t space
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

let jail (player : t) = player.jail

let put_in_jail (player : t) =
  let jail_ = Some 3 in
  {
    name = player.name;
    current_pos = player.current_pos;
    money = player.money;
    owns = player.owns;
    owes_to_bank = player.owes_to_bank;
    jail = jail_;
  }

let get_out_of_jail (player : t) =
  let jail_ = None in
  {
    name = player.name;
    current_pos = player.current_pos;
    money = player.money;
    owns = player.owns;
    owes_to_bank = player.owes_to_bank;
    jail = jail_;
  }

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
  let price = Board.price game space in
  if price > player1.money then raise InsufficientFunds
  else
    let new_funds = player1.money - price in
    let () = Bank.add_funds bank price in
    let new_owns = make_property space 0 0 :: player1.owns in

    {
      name = player1.name;
      current_pos = player1.current_pos;
      money = new_funds;
      owns = new_owns;
      owes_to_bank = player1.owes_to_bank;
      jail = player1.jail;
    }

let buy_house (player : t) (space : int) (game : Board.t) (num_houses : int)
    (bank : Bank.t) : t =
  if num_houses = 0 then player
  else
    let price = num_houses * Board.price_per_house game space in
    if price > player.money then raise InsufficientFunds
    else
      let rec num owns_list space =
        match owns_list with
        | [] -> raise DoesntOwnProperty
        | h :: t ->
            if h.space = space then
              if num_houses > 4 || h.num_houses + num_houses > 4 then
                raise ExceededHouseLimit
              else
                let () = Bank.add_funds bank price in
                add_house
                  { player with money = player.money - price }
                  space game num_houses
            else num t space
      in
      num player.owns space

let buy_hotel (player : t) (space : int) (game : Board.t) (num_hotels : int)
    (bank : Bank.t) =
  if num_hotels = 0 then player
  else
    let price = num_hotels * Board.price_per_hotel game space in
    if price > player.money then raise InsufficientFunds
    else
      let rec num owns_list space =
        match owns_list with
        | [] -> raise DoesntOwnProperty
        | h :: t ->
            if h.space = space then
              if num_hotels > 4 || h.num_hotels + num_hotels > 4 then
                raise ExceededHouseLimit
              else
                let () = Bank.add_funds bank price in
                add_hotel
                  { player with money = player.money - price }
                  space game num_hotels
            else num t space
      in
      num player.owns space

let sell_property (player : t) (space : int) (game : Board.t) =
  failwith "unimplemented"

let sell_house (player : t) (space : int) (game : Board.t) =
  failwith "unimplemented"

let sell_hotel (player : t) (space : int) (game : Board.t) =
  failwith "unimplemented"

let space_of_property p = p.space
let remove_owns (space : int) (player : t) = failwith "Unimplemented"

let property_to_string (input : property) : string =
  match input with
  | { space; num_houses; num_hotels } ->
      "{space =" ^ string_of_int space ^ "; num_houses ="
      ^ string_of_int num_houses ^ "; num_hotels = " ^ string_of_int num_hotels
      ^ "}"

let jail_to_string (input : int option) : string =
  match input with
  | Some x -> "(Some " ^ string_of_int x ^ ")"
  | None -> "None"

let owes_to_bank_to_string (input : int option * int) : string =
  match input with
  | Some x, y -> "(Some " ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  | None, z -> "(None, " ^ string_of_int z ^ ")"

let to_string (player : t) : string =
  match player with
  | { name; current_pos; money; owns; owes_to_bank; jail } ->
      let rec owns_list_to_string owns_list =
        match owns_list with
        | [] -> "]"
        | h :: t -> "[" ^ property_to_string h ^ ";" ^ owns_list_to_string t
      in
      "{name = " ^ name ^ "; current_pos = " ^ string_of_int current_pos
      ^ "; money = " ^ string_of_int money ^ "; owns = "
      ^ owns_list_to_string owns ^ "; owes_to_bank = "
      ^ owes_to_bank_to_string owes_to_bank
      ^ "; jail = " ^ jail_to_string jail ^ "}"
