(************************************************************************)

(* We wrote test cases for all functions in the .mli files corresponding to each
   module. To create the unit tests, we utilized black box testing. To come up
   with test cases, we utilized common or representative cases, and all branches
   of if-statements or pattern matches. When we needed additional assistance
   with debugging, we used glass box testing by adding print statements to the
   functions themselves.

   We chose to playtest the interface instead of writing OUnit tests because it
   would be very difficult or even impossible to write OUnit tests for the
   interface; the interface involves some randomness with the players rolling
   dice.

   An exception to the 'test everything in each module' rule were the card
   decks: the random_card function cannot be tested because it returns a random
   number (hopefully a different random number each time!) and there are too
   many chance and community chest cards to test them all. Therefore, I choose
   some representative cards to test from each, and intend to pay extra
   attention to the others when playtesting.

   We are confident that our testing demonstrates our system's correctness
   because the OUnit tests provide initial evidence that the individual
   functions that make up the system are correct, and playtesting the interface
   allowed for many uncommon scenarios to arise which we were then able to
   debug. After playing through many, many turns of the game, we can be sure
   that it works correctly! *)

(***********************************************************************)

open OUnit2
open Game
open Board
open Command
open State
open Yojson
open Property
open Bank
open Deck

(*********************************************************************)

(* TESTING SUITE FOR BOARD.ML *)

(*********************************************************************)
let board = Board.from_json (Yojson.Basic.from_file "data/board.json")

let test_maker funct name board space expected_output =
  name >:: fun _ -> assert_equal expected_output (funct board space)

let test_maker_exception funct excep name board space =
  name >:: fun _ -> assert_raises excep (fun () -> funct board space)

let name_test = test_maker Board.name
let description_test = test_maker Board.description
let price_test = test_maker Board.price
let price_excep_test = test_maker_exception Board.price Board.SpaceNotOwnable
let rent_test = test_maker Board.rent
let rent_excep_test = test_maker_exception Board.rent Board.SpaceNotOwnable
let salary_test = test_maker Board.salary
let salary_excep_test = test_maker_exception Board.salary NoSalary
let space_type_test = test_maker Board.space_type
let pph_test = test_maker Board.price_per_house
let rph_test = test_maker Board.rent_per_house

let pph_excep_test =
  test_maker_exception Board.price_per_house Board.SpaceNotOwnable

let rph_excep_test =
  test_maker_exception Board.rent_per_house Board.SpaceNotOwnable

let pphot_test = test_maker Board.price_per_hotel
let rphot_test = test_maker Board.rent_per_hotel

let pphot_excep_test =
  test_maker_exception Board.price_per_hotel Board.SpaceNotOwnable

let rphot_excep_test =
  test_maker_exception Board.rent_per_hotel Board.SpaceNotOwnable

let num_spaces_test name board expected_output =
  name >:: fun _ -> assert_equal expected_output (Board.number_of_spaces board)

let between_spaces_test name board sp1 sp2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Board.between_spaces board sp1 sp2)

let monopoly_tests =
  [
    name_test "Space 0 named 'Go'" board 0 "Go";
    name_test "Space 1 named 'Balch Hall" board 1 "Balch Hall";
    description_test "Physical Sciences description" board 5
      "Home of the Goldie's chicken panini.";
    description_test "Go description" board 0
      "You're back at the start of the board.";
    price_test "Four seasons price 220" board 17 220;
    price_excep_test "Go has no price" board 0;
    rent_test "Susp. bridge rent 26" board 25 26;
    rent_excep_test "Go has no rent" board 0;
    salary_test "Go has salary 200" board 0 200;
    salary_excep_test "Balch has no salary" board 1;
    space_type_test "Go has type go" board 0 "go";
    space_type_test "Balch has type property" board 1 "property";
    space_type_test "23 is a Chance" board 23 "chance";
    space_type_test "15 is jail" board 15 "jail";
    space_type_test "3 is a community chest" board 3 "comm-chest";
    num_spaces_test "board has 30 spaces" board 30;
    rph_test "rph of level b is 90" board 16 90;
    pph_test "pph of clock tower is 100" board 13 100;
    rph_excep_test "Go has no rent per house" board 0;
    pph_excep_test "Jail has no price per house" board 15;
    rphot_test "rent per hotel of slope is 200" board 14 200;
    pphot_test "price per hotel of Beebe lake is 400" board 24 400;
    rphot_excep_test "Comm chest has no rent per hotel" board 19;
    pphot_excep_test "Chance has no price per hotel" board 23;
    between_spaces_test "4 spaces from 4 to 8" board 4 8 4;
    between_spaces_test "14 spaces from 20 to 4" board 20 4 14;
    between_spaces_test "0 spaces from 8 to 8" board 8 8 0;
  ]

(*********************************************************************)

(* TESTING SUITE FOR DECK.ML *)

(*********************************************************************)
let chance_deck = Deck.from_json (Yojson.Basic.from_file "data/chance.json")
let comm_deck = Deck.from_json (Yojson.Basic.from_file "data/comm-chest.json")

(* creates basic tests for the deck module *)
let deck_test_maker funct name deck card expected_output =
  name >:: fun _ -> assert_equal expected_output (funct deck card)

let deck_test_maker_exception funct excep name deck space =
  name >:: fun _ -> assert_raises excep (fun () -> funct deck space)

let deck_name_test = deck_test_maker Deck.name
let deck_desc_test = deck_test_maker Deck.description

let num_cards_test name deck expected_output =
  name >:: fun _ -> assert_equal expected_output (Deck.number_cards deck)

let new_space_test = deck_test_maker Deck.new_space

let new_space_wrong_ctype =
  deck_test_maker_exception Deck.new_space Deck.IncorrectCardType

let comm_info_test = deck_test_maker Deck.comm_chest_info

let comm_info_wrongtype =
  deck_test_maker_exception Deck.comm_chest_info Deck.IncorrectCardType

let deck_tests =
  [
    deck_name_test "Chance card 1 called 'Beautiful Sunset'" chance_deck 1
      "Beautiful Sunset";
    deck_name_test "Comm chest card 14 called 'Finance Bro'" comm_deck 14
      "Finance Bro";
    deck_desc_test "Chance card 8 desc office hours" chance_deck 8
      "You need some help on your 3110 assignment. Go to office hours in \
       Rhodes Hall.";
    deck_desc_test "Comm chest card 4 desc fin. aid" comm_deck 4
      "Financial aid packages are released today! Take $100.";
    num_cards_test "17 cards in chance deck" chance_deck 17;
    num_cards_test "17 cards in comm deck" comm_deck 17;
    new_space_test "new space of chance 0 is 15" chance_deck 0 (Some 15);
    new_space_test "new space of chance 6 is None" chance_deck 6 None;
    new_space_wrong_ctype "comm chest card no new space" comm_deck 7;
    comm_info_test "Card 16 type other" comm_deck 16 Other;
    comm_info_test "Card 13 earn 10" comm_deck 13 (Earn (Some 10));
    comm_info_test "Card 12 pay 10" comm_deck 12 (Pay (Some 10));
    comm_info_wrongtype "chance deck wrong type" chance_deck 11;
  ]

(* [print_command command] is a string representation of [command] of type
   command.*)
let print_command command =
  match command with
  | Go -> "go"
  | Yes -> "yes"
  | No -> "no"
  | Quit -> "quit"
  | Player_name t -> "player name" ^ t
  | Number_of_players t -> "number of players" ^ string_of_int t

let parse_test (name : string) (str : string) (expected_output : command) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (parse str) ~printer:print_command

let command_tests =
  [
    parse_test "text input is go, result is Go" "go" Go;
    parse_test "text input is yes, result is Yes" "yes" Yes;
    parse_test "text input is no, result is No" "no" No;
    parse_test "text input is Prakriti, result is Player_name Prakriti"
      "Prakriti" (Player_name "Prakriti");
    parse_test "text input is 5, result is Number_of_Players 5" "5"
      (Number_of_players 5);
  ]

(******************************************************************************
  state.ml tests
  ******************************************************************************)

let name_test (name : string) (t : State.t) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.name t) ~printer:(fun s -> s)

let current_pos_test (name : string) (t : State.t) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (State.current_pos t) ~printer:string_of_int

let current_balance_test (name : string) (t : State.t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (State.current_balance t) ~printer:string_of_int

(*once implement owns function, add State.owns player for value of owns field *)
let print_player_state (player : State.t) : string =
  "{name = " ^ State.name player ^ "; current_pos = "
  ^ string_of_int (State.current_pos player)
  ^ "; money = "
  ^ string_of_int (State.current_balance player)
  ^ "; owns = [] }"

let change_balance_test (name : string) (t : State.t) (amt : int)
    (expected_balance_output : int)
      (*(expected_name : string) (expected_cur_pos : int) (expected_owns : int
        list)*) =
  name >:: fun _ ->
  (*assert_bool "The name field was accidentally changed and doesn't match what
    it was \ before" (State.name (State.change_balance t amt) = expected_name);
    assert_bool "The cur_pos field was accidentally changed and doesn't match
    what it was \ before" State.current_pos (State.change_balance t amt) =
    expected_cur_pos; (*TODO: implement owns function and write this assert_bool
    statement: assert_bool "The owns field was accidentally changed and doesn't
    match what it was \ before" (State.owns (State.change_balance t amt) =
    expected_owns);*)*)
  assert_equal expected_balance_output
    (State.current_balance (State.change_balance t amt))
    ~printer:string_of_int

let check_name (name : string) (t : State.t) (amt : int)
    (expected_balance_output : int) (expected_name_output : string) =
  name >:: fun _ ->
  assert_equal expected_name_output
    (State.name (State.change_balance t amt))
    ~printer:Fun.id

(* checks whether the new game state created from buy_property has the owner of
   the property just bought equal to the player who bought the property. If an
   illegal exception was raised, check that the game wasn't affected.*)
let check_game (name : string) (player : State.t) =
  raise (Failure "Implement me")

let game_board = Board.from_json (Yojson.Basic.from_file "data/board.json")
let bank1 = Bank.init_bank 5000
let prop1 = State.make_property 1 0 0
let prop2 = State.make_property 2 0 0
let state_one = State.init_state "Prakriti"

let state_two =
  State.buy_property state_one 28 game_board bank1 (*adds $350 to bank*)

let state_owns_prop_one =
  State.buy_property state_one 1 game_board bank1 (*adds $60 to bank*)

let state_owns_prop_five = State.change_owns 5 (State.init_state "Prakriti")
let state_three = change_owns 1 state_one
let state_four = change_owns 5 state_three
let go_state = go 2 state_one game_board

(*let player_two = buy_property (State.init_state "Amy") 2 game_board
  (Bank.init_bank 5000)*)

(*let rent_play1 = (pay_rent go_state player_two game_board).player1 let
  rent_play2 = (pay_rent go_state player_two game_board).player2*)
(*let player_two_insuf_funds = State.buy_property player_two 22 game_board
  bank1*)

let make_owns_test (name : string) (player1 : State.t) (space : int)
    (game : Board.t) (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (State.owns player1 space game)

let buy_property_test (name : string) (player1 : State.t) (space : int)
    (game : Board.t) (bank : Bank.t) (expected_output : State.t) =
  name >:: fun _ ->
  assert_equal expected_output
    (buy_property player1 space game bank)
    ~printer:State.to_string

let buy_property_exception_test (name : string) (player1 : State.t)
    (space : int) (game : Board.t) (bank : Bank.t) =
  name >:: fun _ ->
  assert_raises State.InsufficientFunds (fun () ->
      buy_property player1 space game bank)

let make_compare_property_test (name : string) (prop1 : property)
    (prop2 : property) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (State.compare_property prop1 prop2)

let make_owns_list_test (name : string) (player1 : State.t)
    (expected_output : property list) =
  name >:: fun _ -> assert_equal expected_output (State.owns_list player1)

let make_num_houses_test (name : string) (player1 : State.t) (space : int)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (State.num_houses player1 space)

let make_num_houses_exception_test (name : string) (player1 : State.t)
    (space : int) =
  name >:: fun _ ->
  assert_raises DoesntOwnProperty (fun () -> num_houses player1 space)

let make_buy_house_test (name : string) (player1 : State.t) (space : int)
    (game : Board.t) (num_houses : int) (bank : Bank.t)
    (expected_output : State.t) =
  name >:: fun _ ->
  assert_equal expected_output
    (State.buy_house player1 space game num_houses bank)
    ~printer:State.to_string

let make_buy_house_exception_test (name : string) (player1 : State.t)
    (space : int) (game : Board.t) (num_houses : int) (bank : Bank.t)
    (expected_output : State.t) =
  name >:: fun _ ->
  assert_raises ExceededHouseLimit (fun () ->
      State.buy_house player1 space game num_houses bank)

let make_num_hotels_test (name : string) (player1 : State.t) (space : int)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (State.num_hotels player1 space)

let make_num_hotels_exception_test (name : string) (player1 : State.t)
    (space : int) =
  name >:: fun _ ->
  assert_raises DoesntOwnProperty (fun () -> num_hotels player1 space)

let make_buy_hotel_test (name : string) (player1 : State.t) (space : int)
    (game : Board.t) (num_hotels : int) (bank : Bank.t)
    (expected_output : State.t) =
  name >:: fun _ ->
  assert_equal expected_output
    (State.buy_hotel player1 space game num_hotels bank)
    ~printer:State.to_string

let make_buy_hotel_exception_test (name : string) (player1 : State.t)
    (space : int) (game : Board.t) (num_hotels : int) (bank : Bank.t)
    (expected_output : State.t) =
  name >:: fun _ ->
  assert_raises ExceededHouseLimit (fun () ->
      State.buy_hotel player1 space game num_hotels bank)

let custom_owes_to_bank_printer (input : int option * int) : string =
  match input with
  | Some x, y -> "(Some " ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  | None, z -> "(None, " ^ string_of_int z ^ ")"

let make_owes_to_bank_test (name : string) (player1 : State.t)
    (expected_output : int option * int) =
  name >:: fun _ ->
  assert_equal expected_output
    (State.owes_to_bank player1)
    ~printer:custom_owes_to_bank_printer

let custom_jail_printer (input : int option) : string =
  match input with
  | Some x -> "Some " ^ string_of_int x
  | None -> "None"

let make_jail_test (name : string) (player1 : State.t)
    (expected_output : int option) =
  name >:: fun _ ->
  assert_equal expected_output (State.jail player1) ~printer:custom_jail_printer

let make_remove_owns_test (name : string) (space : int) (player1 : State.t)
    (expected_output : State.t) =
  name >:: fun _ ->
  assert_equal expected_output
    (State.remove_owns space player1)
    ~printer:State.to_string

let make_turn_in_debt_test (name : string) (player1 : State.t) (turns : int)
    (expected_output : State.t) =
  name >:: fun _ ->
  assert_equal expected_output
    (State.turn_in_debt player1 turns)
    ~printer:State.to_string

let state_tests =
  [
    name_test "name of state_one is Prakriti" state_one "Prakriti";
    current_pos_test "current position of state_one is 0" state_one 0;
    make_owns_test "player with no properties, space 1, game" state_one 1
      game_board false;
    (*--------------------following test checks cur_balance &
      change_balance---*)
    current_balance_test "current_balance is 500" state_one 500;
    change_balance_test "deduct $200 from original balance: $500" state_one
      (-200) 300;
    current_balance_test "current_balance is 300"
      (State.change_balance state_one (-200))
      300;
    change_balance_test "add $200 to original balance: $500" state_one (-200)
      300;
    change_balance_test "add $0 to original balance: $500" state_one 0 500;
    check_name "deduct $200 from original balance: $500" state_one (-200) 300
      "Prakriti";
    (*--------------------following test checks buy_property-----------------*)
    make_owns_test "player with owns = [28], space 1, game" state_two 28
      game_board true;
    make_owns_test "player with owns=[1], space 2, game" state_two 2 game_board
      false;
    current_balance_test "$200 remaining"
      (buy_property state_one 25 game_board bank1)
      200;
    buy_property_test "owns 1 prop at space 1" state_one 1 game_board bank1
      state_owns_prop_one;
    buy_property_exception_test
      "player with $200 tries to buy property with price $400"
      (buy_property state_one 25 game_board bank1)
      29 game_board bank1;
    (*---------------------following test checks compare_property-------------*)
    make_compare_property_test "prop1 = prop 1" prop1 prop1 0;
    make_compare_property_test "prop1 < prop 2" prop1 prop2 (-1);
    make_compare_property_test "prop1 > prop 2" prop2 prop1 1;
    (*---------------------following test checks owns_list-------------------*)
    make_owns_list_test "owns no properties" state_one [];
    make_owns_list_test "owns 1 property {space=1; num_houses=0; num_hotels=0}"
      state_three
      [ State.make_property 1 0 0 ];
    (*---------------------following test checks change_owns------------------*)
    make_owns_test "player with owns = [1]" state_three 1 game_board true;
    make_owns_test "player with owns = [5]" state_four 5 game_board true;
    (*---------------------following test checks remove_owns--------------*)
    make_remove_owns_test
      "removes prop at space 1 -> player has no properties remaining" 1
      state_owns_prop_one
      (State.change_balance state_one (-60));
    make_remove_owns_test
      "removes prop at space 1 -> player has 1 property remaining" 1 state_four
      state_owns_prop_five;
    (*-------------------following test checks go--------------------------*)
    current_pos_test "current pos of state_one after it has moved 2 steps is 2"
      go_state 2;
    (*----------following test checks num_houses-----------*)
    make_num_houses_exception_test "no properties" state_one 1;
    make_num_houses_test "owns prop at space 1, 0 houses" state_three 1 0;
    (*----------following test checks buy_house-----------*)
    make_buy_house_test "buy 0 houses" state_two 28 game_board 0 bank1 state_two;
    make_buy_house_test "buy 1 house, price:$50" state_owns_prop_one 1
      game_board 1 bank1
      (State.add_house
         (change_balance state_owns_prop_one (-50))
         1 game_board 1);
    make_buy_house_test "buy 2 houses, price:$100" state_owns_prop_one 1
      game_board 2 bank1
      (State.add_house
         (change_balance state_owns_prop_one (-100))
         1 game_board 2);
    (*----------following test checks num_hotels-----------*)
    make_num_hotels_exception_test "no properties" state_one 1;
    make_num_hotels_test "owns prop at space 1, 0 hotels" state_three 1 0;
    (*----------following test checks buy_hotels-----------*)
    make_buy_hotel_test "buy 0 hotels" state_two 28 game_board 0 bank1 state_two;
    make_buy_hotel_test "buy 1 hotel, price:$100" state_owns_prop_one 1
      game_board 1 bank1
      (State.add_hotel
         (change_balance state_owns_prop_one (-100))
         1 game_board 1)
    (*----------following test checks owes_to_bank-----------*);
    make_owes_to_bank_test "owes nothing to bank (None, 0)" state_one (None, 0);
    make_owes_to_bank_test "owes $500 to bank (500, 2)"
      (change_owes state_one 500)
      (Some 500, 2);
    make_owes_to_bank_test "owes $500 to bank (500, 2), but pays it all off"
      (change_owes (change_owes state_one 500) (-500))
      (None, 0);
    (*need to check that y value decremets each time go around go, need to
      implement go module before can test this*)
    (*make_owes_to_bank_test "owes $500 to bank and went around go 1x (500, 1)"
      (go 31 (change_owes state_one 500) game_board) (Some 500, 1);
      make_owes_to_bank_test "owes $500 to bank and went around go 2x (500, 0)"
      (go 61 (change_owes state_one 500) game_board) (Some 500, 0);*)
    (*----------following test checks turn_in_debt-----------*)
    make_turn_in_debt_test "have 2 remaining turns left to pay loan"
      (change_owes state_one 500)
      0
      (change_owes state_one 500);
    (*make_turn_in_debt_test "have 1 remaining turns left to pay loan"
      (change_owes state_one 500) 1 (change_owes state_one 500);
      make_turn_in_debt_test "have 0 remaining turns left to pay loan"
      (change_owes state_one 500) 2 (change_owes state_one 500);*)
    (*----------following test checks jail, put_in_jail,get_out_of_jail-----*)
    make_jail_test "not in jail" state_one None;
    make_jail_test "in jail, has 3 turns remaining in jail"
      (put_in_jail state_one) (Some 3);
    make_jail_test "get player who has 3 turns remaining in jail"
      (get_out_of_jail (put_in_jail state_one))
      None;
    (*need to implement go module before can test this*)
    (*make_jail_test "in jail, has 2 turns remaining in jail" (put_in_jail (go
      31 state_one game_board)) (Some 2); make_jail_test "in jail, has 1 turns\n
      remaining in jail" (put_in_jail (go 61 state_one game_board)) (Some 1);*)
    (*----------following test checks turn_in_jail-----*)
    make_jail_test "in jail, has 2 turns remaining in jail"
      (State.turn_in_jail (put_in_jail state_one) 1)
      (Some 2);
    make_jail_test "in jail, has 1 turns remaining in jail"
      (State.turn_in_jail (put_in_jail state_one) 2)
      (Some 1);
    make_jail_test "in jail, has 0 turns remaining in jail"
      (State.turn_in_jail (put_in_jail state_one) 3)
      None;
  ]

(******************************************************************************
  Bank.ml tests
  ******************************************************************************)
let bank_init = Bank.init_bank 5000
let bank_1 = Bank.init_bank 5000
let bank_2 = Bank.init_bank 5000

let bank_b =
  Bank.add_funds bank_2 50;
  bank_2

let bank_3 = Bank.init_bank 5000

let bank_r =
  Bank.deduct_funds bank_3 2;
  bank_3

let test_bank_with_state =
  State.buy_property state_one 2 game_board bank_1 (*adds $60 to bank_1*)

let make_funds_test (name : string) (bank : Bank.t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (Bank.funds bank) ~printer:string_of_int

let bank_tests =
  [
    (*---------------------following test checks funds-------------*)
    make_funds_test "Bank initially has $5000" bank_init 5000;
    make_funds_test "+60 to bank, funds: $5060" bank_1 5060;
    (*---------------------following test checks add_funds-------------*)
    make_funds_test "Bank_2 add 5" bank_b 5050;
    (*---------------------following test checks deduct_funds-------------*)
    make_funds_test "Bank_1 remove 10" bank_r 4998;
  ]

(******************************************************************************
  Property.ml tests
  ******************************************************************************)

let player1 = State.go 1 (State.init_state "A") board
let player2 = State.go 1 (State.init_state "B") board
let player3 = State.go 1 (State.init_state "C") board
let pls = [| player1; player2; player3 |]

let print_property_status (st : Property.status) =
  match st with
  | NotOwned -> "NotOwned"
  | OwnedByOtherPlayer a -> "OwnedByOtherPlayer" ^ State.name a
  | OwnedByThisPlayer -> "OwnedByThisPlayer"

(* let status_equal (st1 : Property.status) (st2 : Property.status) = match st1
   with | NotOwned -> begin match st2 with | NotOwned -> true | _ -> false end |
   OwnedByThisPlayer -> begin match st2 with | OwnedByThisPlayer -> true | _ ->
   false end | OwnedByOtherPlayer a -> begin match st2 with | OwnedByOtherPlayer
   b -> State.name a = State.name b | _ -> false end *)

let property_status_test (name : string) (pls : Property.player_list)
    (curr_pl : State.t) (board : Board.t) (expected_ouptut : string) =
  name >:: fun _ ->
  assert_equal
    (print_property_status (property_status pls curr_pl board))
    expected_ouptut
    ~printer:(fun x -> x)

let determine_rent_test (name : string) (owner : State.t) (property : int)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  (* print_string (string_of_bool (State.owns owner property board));
     print_string (string_of_int (State.num_houses owner)) *)
  assert_equal
    (determine_rent owner property board)
    expected_output ~printer:string_of_int

let find_index_test (name : string) (pls : player_list) (pl_name : string)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal (find_index pl_name pls) expected_output ~printer:string_of_int

let determine_price_test (name : string) (owner : State.t) (property : int)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal
    (determine_price owner property board)
    expected_output ~printer:string_of_int

(*checks the balance of the player who pays the rent*)
let pay_rent_test_payer (name : string) (pls : player_list) (curr_pl : State.t)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  pay_rent pls curr_pl board;
  assert_equal
    (pls.(find_index (State.name curr_pl) pls) |> current_balance)
    expected_output ~printer:string_of_int

let get_owner pls curr_pl board =
  match property_status pls curr_pl board with
  | OwnedByOtherPlayer pl -> pl
  | _ -> failwith "impossible"

let get_same_owner pls curr_pl board =
  match property_status pls curr_pl board with
  | OwnedByThisPlayer -> State.name curr_pl
  | _ -> failwith "precondition violated"

(*check the new balance of the person who owns the property*)
let pay_rent_test_owner (name : string) (pls : player_list) (curr_pl : State.t)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  pay_rent pls curr_pl board;
  assert_equal
    (get_owner pls curr_pl board |> current_balance)
    expected_output ~printer:string_of_int

(*checks the account balance of the original owner of property who gained
  money*)
let buy_property_from_player_test_owner (name : string) (pls : player_list)
    (curr_pl : State.t) (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  buy_property_from_player pls curr_pl board;
  assert_equal
    (get_owner pls curr_pl board |> current_balance)
    expected_output ~printer:string_of_int

(*checks the new ownership of the player who bought the property*)
let buy_property_from_player_test_payer (name : string) (pls : player_list)
    (curr_pl : State.t) (board : Board.t) (expected_output : string) =
  name >:: fun _ ->
  buy_property_from_player pls curr_pl board;
  assert_equal (get_same_owner pls curr_pl board) expected_output
    ~printer:(fun x -> x)

(* checks the new balance of the player who bought the property*)
let buy_property_from_player_test_balance (name : string) (pls : player_list)
    (curr_pl : State.t) (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  buy_property_from_player pls curr_pl board;
  assert_equal
    (pls.(find_index (State.name curr_pl) pls) |> current_balance)
    expected_output ~printer:string_of_int

(*checks that the player state has been updated by comparing the current
  balance*)
let update_player_test (name : string) (pls : player_list) (old_pl : State.t)
    (new_pl : State.t) (expected_output : int) =
  name >:: fun _ ->
  update_player pls old_pl new_pl;
  assert_equal
    (pls.(find_index (State.name new_pl) pls) |> current_balance)
    expected_output ~printer:string_of_int

(*checks that the player state has been updated by comparing the new owner*)
let update_player_test_space (name : string) (pls : player_list)
    (old_pl : State.t) (new_pl : State.t) (space : int) (board : Board.t)
    (expected_output : bool) =
  name >:: fun _ ->
  update_player pls old_pl new_pl;
  assert_equal
    (owns pls.(find_index (State.name new_pl) pls) space board)
    expected_output ~printer:string_of_bool

let pls1 = [| player1; player2; player3 |]

let () =
  update_player pls1 player1 (buy_property player1 1 board (Bank.init_bank 500))

let player1_owner1 = buy_property player1 1 board (Bank.init_bank 500)

let property_tests =
  [
    (*----------------following test checks update_player-----------------*)
    update_player_test "old player had 500, new player has 505" pls player1
      (change_balance player1 5) 505;
    update_player_test_space "old player had 500, new player has 500" pls
      player1
      (buy_property player1 1 board (Bank.init_bank 500))
      1 board true;
    (*----------------following test checks property_status-----------------*)
    property_status_test "owned by no one"
      [| player1; player2; player3 |]
      player1 board "NotOwned";
    property_status_test "Owned by other player" pls1 player2 board
      ("OwnedByOtherPlayer" ^ State.name player1);
    property_status_test "Owned by this player" pls1 player1 board
      "OwnedByThisPlayer"
    (*----------------following test checks determine_rent-----------------*);
    determine_rent_test "rent of 1 is 2" player1_owner1 1 board 2;
    (*----------------following test checks find_index-----------------*)
    find_index_test "index of player2 is 1" pls "B" 1;
    (*----------------following test checks determine_price-----------------*)
    determine_price_test "price of 1 is 60" player1_owner1 1 board 60;
    (*----------------following test checks pay_rent-----------------*)
    pay_rent_test_owner "player1 gains 2" pls1 player2 board 442;
    pay_rent_test_payer "player2 loses 2" pls1 player2 board 498;
    (*----------------following test checks buy_property-----------------*)
    buy_property_from_player_test_balance "player2 bought 1 from player1" pls1
      player2 board 440;
    buy_property_from_player_test_payer
      "player2 bought 1 from player1 - check ownership" pls1 player2 board "B";
    buy_property_from_player_test_owner "check balance of player1" pls1 player2
      board 500;
  ]

(*************************************************************************)

(**** Tests for Community Chest and Chance cards! *)

(*************************************************************************)

let player4 = State.go 7 (State.init_state "special") board
let player5 = State.go 23 (State.init_state "comm_chest") board

(* re-initializing in case previous tests messed with the players. *)
let pls = [| player1; player2; player3 |]

(* needed a second after manipulating *)
let pls2 = [| player1; player2; player3 |]
let pls3 = [| player1; player4; player5 |]
let bank = Bank.init_bank 5000

let comm_chest_earn_test name deck pls bank i player exp_balance_after =
  name >:: fun _ ->
  let plname = State.name player in
  let plindex = Property.find_index plname pls in
  Comm_chest.exec_card deck pls bank board i player;
  assert_equal (current_balance pls.(plindex)) exp_balance_after

let all_bal_change_test name deck pls bank i player (bals : int array) =
  name >:: fun _ ->
  let plname = State.name player in
  let plindex = Property.find_index plname pls in
  Comm_chest.exec_card deck pls bank board i player;
  assert_equal ~msg:"current player"
    (current_balance pls.(plindex))
    bals.(plindex);
  assert_equal ~msg:"player 1" (current_balance pls.(0)) bals.(0);
  assert_equal ~msg:"player 2" (current_balance pls.(1)) bals.(1);
  assert_equal ~msg:"player 3" (current_balance pls.(2)) bals.(2)

let chance_move_test name deck pls bank i player exp_pos_after =
  name >:: fun _ ->
  let plname = State.name player in
  let plindex = Property.find_index plname pls in
  Chance.exec_card deck pls bank board i player;
  assert_equal (current_pos pls.(plindex)) exp_pos_after

(* It is not practical to test every single community chest card - these 3 cases
   are representative of common card types and I will play test the rest.*)
let comm_chance_tests =
  [
    comm_chest_earn_test "finance bro - earn 1000" comm_deck pls bank 14 player1
      1500;
    (* tests Earn card *)
    comm_chest_earn_test "ryan lombardi - pay 10" comm_deck pls bank 12 player2
      490;
    (* tests Pay card *)
    all_bal_change_test "slope day pregame test" comm_deck pls2 bank 7 player1
      [| 410; 410; 410 |];
    chance_move_test "quarantine card -> statler hotel" chance_deck pls bank 10
      player1 22;
    chance_move_test "double chance" chance_deck pls3 bank 13 player4 23;
    chance_move_test "double community chest" chance_deck pls3 bank 14 player5
      27;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           monopoly_tests;
           deck_tests;
           command_tests;
           state_tests;
           property_tests;
           comm_chance_tests;
           bank_tests;
         ]

let _ = run_test_tt_main suite
