type t = {
  name : string;
  current_pos : int;
  money : int;
  owns : int list;
}

type combined_state = {
  player1 : t;
  player2 : t;
}

exception InsufficientFunds

let init_state str = { name = str; current_pos = 0; money = 500; owns = [] }
let name (player : t) = player.name
let current_pos (player : t) = player.current_pos
let current_balance (player : t) = player.money

let change_balance (player : t) (amt : int) =
  let new_amt = player.money + amt in
  {
    name = player.name;
    current_pos = player.current_pos;
    money = new_amt;
    owns = player.owns;
  }

let rec owns (player : t) (space : int) (game : Board.t) =
  match player.owns with
  | [] -> false
  | h :: t ->
      if h = space then true else owns { player with owns = t } space game

let change_owns pos play1 =
  { play1 with owns = List.sort_uniq Int.compare (pos :: play1.owns) }

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
  }

let pay_rent play1 play2 game =
  let rent = Board.rent game (current_pos play1) in
  if play1.money < rent then raise InsufficientFunds
  else
    {
      player1 = { play1 with money = play1.money - rent };
      player2 = { play2 with money = play2.money + rent };
    }

let buy_property (player1 : t) (space : int) (game : Board.t) =
  let new_funds = player1.money - Board.price game space in
  if new_funds < 0 then raise InsufficientFunds
  else
    let new_owns = player1.owns @ [ space ] in

    {
      name = player1.name;
      current_pos = player1.current_pos;
      money = new_funds;
      owns = new_owns;
    }
