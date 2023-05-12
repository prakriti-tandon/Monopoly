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

let new_space_nonewspace =
  deck_test_maker_exception Deck.new_space Deck.NoNewSpace

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
    new_space_test "new space of chance 0 is 15" chance_deck 0 15;
    new_space_wrong_ctype "comm chest card no new space" comm_deck 7;
    new_space_nonewspace "chance card 11 has no new space" chance_deck 11;
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
let state_one = State.init_state "Prakriti"

(*let state_two = State.buy_property state_one 1 game_board (Bank.init_bank
  5000)*)
let state_three = change_owns 1 state_one
let go_state = go 2 state_one game_board

(*let player_two = buy_property (State.init_state "Amy") 2 game_board
  (Bank.init_bank 5000)*)

let bank1 = Bank.init_bank 5000

(*let rent_play1 = (pay_rent go_state player_two game_board).player1 let
  rent_play2 = (pay_rent go_state player_two game_board).player2*)
(*let player_two_insuf_funds = State.buy_property player_two 22 game_board
  bank1*)

let make_owns_test (name : string) (player1 : State.t) (space : int)
    (game : Board.t) (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (State.owns player1 space game)

let buy_property_exception_test (name : string) (player1 : State.t)
    (space : int) (game : Board.t) =
  name >:: fun _ ->
  assert_raises InsufficientFunds (fun () -> buy_property player1 space game)

let make_num_houses_test (name : string) (player1 : State.t) (space : int)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (State.num_houses player1 space)

let make_num_houses_exception_test (name : string) (player1 : State.t)
    (space : int) =
  name >:: fun _ ->
  assert_raises DoesntOwnProperty (fun () -> num_houses player1 space)

let state_tests =
  [
    name_test "name of state_one is Prakriti" state_one "Prakriti";
    current_pos_test "current position of state_one is 0" state_one 0;
    current_balance_test "current_balance is 500" state_one 500;
    change_balance_test "deduct $200 from original balance: $500" state_one
      (-200) 300;
    current_balance_test "current_balance is 300"
      (State.change_balance state_one (-200))
      300;
    check_name "deduct $200 from original balance: $500" state_one (-200) 300
      "Prakriti";
    change_balance_test "add $200 to original balance: $500" state_one (-200)
      300;
    change_balance_test "add $0 to original balance: $500" state_one 0 500;
    make_owns_test "player with no properties, space 1, game" state_one 1
      game_board false;
    (*--------------------following test checks buy_property-----------------*)
    (* make_owns_test "player with owns = [1], space 1, game" state_two 1
       game_board true; (*following test checks buy_property*) make_owns_test
       "player with owns=[1], space 2, game" state_two 2 game_board false;*)
    (*---------------------following test checks change_owns------------------*)
    make_owns_test "player with owns = [1]" state_three 1 game_board true;
    (*-------------------following test checks go--------------------------*)
    current_pos_test "current pos of state_one after it has moved 2 steps is 2"
      go_state 2;
    (*----------following test checks num_houses-----------*)
    make_num_houses_exception_test "no properties" state_one 1;
    make_num_houses_test "owns prop at space 1, 0 houses" state_one 1 0;
  ]

(******************************************************************************
  Bank.ml tests
  ******************************************************************************)

(******************************************************************************
  Property.ml tests
  ******************************************************************************)

let player1 = State.go 1 (State.init_state "A") board
let player2 = State.go 1 (State.init_state "B") board
let player3 = State.go 1 (State.init_state "C") board
let pls = [| player1; player2; player3 |]

let property_status_test (name : string) (pls : Property.player_list)
    (curr_pl : State.t) (board : Board.t) (expected_ouptut : Property.status) =
  name >:: fun _ ->
  assert_equal (property_status pls curr_pl board) expected_ouptut

let determine_rent_test (name : string) (owner : State.t) (property : int)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal (determine_rent owner property board) expected_output

let find_index_test (name : string) (pls : player_list) (pl_name : string)
    (expected_output : int) =
  name >:: fun _ -> assert_equal (find_index pl_name pls) expected_output

let determine_price_test (name : string) (owner : State.t) (property : int)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal (determine_price owner property board) expected_output

(*checks the balance of the player who pays the rent*)
let pay_rent_test_payer (name : string) (pls : player_list) (curr_pl : State.t)
    (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  pay_rent pls curr_pl board;
  assert_equal
    (pls.(find_index (State.name curr_pl) pls) |> current_balance)
    expected_output

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
  assert_equal (get_owner pls curr_pl board |> current_balance) expected_output

(*checks the account balance of the original owner of property who gained
  money*)
let buy_property_from_player_test_owner (name : string) (pls : player_list)
    (curr_pl : State.t) (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  buy_property_from_player pls curr_pl board;
  assert_equal (get_owner pls curr_pl board |> current_balance) expected_output

(*checks the new ownership of the player who bought the property*)
let buy_property_from_player_test_payer (name : string) (pls : player_list)
    (curr_pl : State.t) (board : Board.t) (expected_output : string) =
  name >:: fun _ ->
  buy_property_from_player pls curr_pl board;
  assert_equal (get_same_owner pls curr_pl board) expected_output

(* checks the new balance of the player who bought the property*)
let buy_property_from_player_test_balance (name : string) (pls : player_list)
    (curr_pl : State.t) (board : Board.t) (expected_output : int) =
  name >:: fun _ ->
  buy_property_from_player pls curr_pl board;
  assert_equal
    (pls.(find_index (State.name curr_pl) pls) |> current_balance)
    expected_output

(*checks that the player state has been updated by comparing the current
  balance*)
let update_player_test (name : string) (pls : player_list) (old_pl : State.t)
    (new_pl : State.t) (expected_output : int) =
  name >:: fun _ ->
  update_player pls old_pl new_pl;
  assert_equal
    (pls.(find_index (State.name new_pl) pls) |> current_balance)
    expected_output

(*checks that the player state has been updated by comparing the new owner*)
let update_player_test_space (name : string) (pls : player_list)
    (old_pl : State.t) (new_pl : State.t) (space : int) (board : Board.t)
    (expected_output : bool) =
  name >:: fun _ ->
  update_player pls old_pl new_pl;
  assert_equal
    (owns pls.(find_index (State.name new_pl) pls) space board)
    expected_output

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
      player1 board NotOwned;
    property_status_test "Owned by other player" pls (go 1 player2 board) board
      (OwnedByOtherPlayer player1);
    property_status_test "Owned by this player" pls (go 1 player1 board) board
      OwnedByThisPlayer;
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
         ]

let _ = run_test_tt_main suite
