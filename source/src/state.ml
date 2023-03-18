type t = {
  name : string;
  current_pos : int;
  money : int;
  owns : int list;
}

type combined_state = {
  player1 : t;
  player2 : t;
  game : Monopoly.t;
}

type result =
  | Illegal
  | Legal of combined_state

let init_state str = { name = str; current_pos = 0; money = 500; owns = [] }
let name (player : t) = player.name
let current_pos (player : t) = player.current_pos
let current_balance (player : t) = player.money

let change_balance (player : t) (amt : int) =
  let new_amount = player.money + amt in
  {
    name = player.name;
    current_pos = player.current_pos;
    money = new_amount;
    owns = player.owns;
  }

let owns (player : t) (space : int) (game : Monopoly.t) =
  raise (Failure "Implement me")

let change_owns (pos : int) (player : t) =
  {
    name = player.name;
    current_pos = player.current_pos;
    money = player.money;
    owns = List.sort_uniq Int.compare (pos :: player.owns);
  }

let go (player : t) (game : Monopoly.t) =
  let dice_result = Random.int 7 in
  let result_position =
    (player.current_pos + dice_result) mod Monopoly.number_of_spaces game
  in
  {
    name = player.name;
    current_pos = result_position;
    money = player.money;
    owns = player.owns;
  }

let pay_rent play1 play2 game =
  let rent = Monopoly.rent game (current_pos play1) in
  if play1.money < rent then Illegal
  else
    Legal
      {
        player1 = { play1 with money = play1.money - rent };
        player2 = { play2 with money = play2.money + rent };
        game;
      }

let buy_property (player : t) (space : int) (game : Monopoly.t) =
  raise (Failure "Implement me")
