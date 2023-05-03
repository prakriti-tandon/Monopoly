open OUnit2
open Game
open Monopoly
open Command
open State
open Yojson

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list] to get
   helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] )
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo"; "foo"]
       ["foo"]); *);
  ]

(**************************************************************)
let board = Monopoly.from_json (Yojson.Basic.from_file "data/board.json")

let test_maker funct name board space expected_output =
  name >:: fun _ -> assert_equal expected_output (funct board space)

let test_maker_exception funct excep name board space =
  name >:: fun _ -> assert_raises excep (fun () -> funct board space)


let name_test = test_maker Monopoly.name
let description_test = test_maker Monopoly.description

let price_test = test_maker Monopoly.price
let price_excep_test = test_maker_exception Monopoly.price SpaceNotOwnable
let rent_test = test_maker Monopoly.rent
let rent_excep_test = test_maker_exception Monopoly.rent SpaceNotOwnable
let salary_test = test_maker Monopoly.salary
let salary_excep_test = test_maker_exception Monopoly.salary NoSalary
let space_type_test = test_maker Monopoly.space_type

let num_spaces_test name board expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.number_of_spaces board)

let monopoly_tests =
  [
    
    name_test "Space 0 named 'Go'" board 0 "Go";
    name_test "Space 1 named 'Balch Hall" board 1 "Balch Hall";
    description_test "Physical Sciences description" board 4
      "Home of the Goldie's chicken panini.";
    description_test "Go description" board 0 "You're back at the start of the board.";
    price_test "Four seasons price 220" board 13 220;
    price_excep_test "Go has no price" board 0;
    rent_test "Susp. bridge rent 26" board 19 26;
    rent_excep_test "Go has no rent" board 0;
    salary_test "Go has salary 200" board 0 200;
    salary_excep_test "Balch has no salary" board 1;
    space_type_test "Go has type go" board 0 "go";
    space_type_test "Balch has type property" board 1 "property";
    num_spaces_test "board has 23 spaces" board 23;
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

let game_board = Monopoly.from_json (Yojson.Basic.from_file "data/board.json")
let state_one = init_state "Prakriti"
let state_two = State.buy_property state_one 1 game_board
let state_three = change_owns 1 state_one
let go_state = go 2 state_one game_board
let player_two = buy_property (init_state "Amy") 2 game_board

let rent_play1 = (pay_rent go_state player_two game_board).player1
let rent_play2 = (pay_rent go_state player_two game_board).player2

let player_two_insuf_funds= (buy_property player_two 22 game_board) 

let make_owns_test (name : string) (player1 : State.t) (space : int)
    (game : Monopoly.t) (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (State.owns player1 space game)

let buy_property_exception_test (name :string) (player1: State.t) (space:int) (game: Monopoly.t) = 
  name >:: (fun _ -> assert_raises (InsufficientFunds) (fun () -> buy_property player1 space game))

let make_num_houses_test (name: string) (player1 :State.t) (space: int) (expected_output:int)= 
name >:: fun _ -> assert_equal expected_output (State.num_houses player1 space)
let state_tests =
  [
    name_test "name of state_one is Prakriti" state_one "Prakriti";
    current_pos_test "current position of state_one is 0" state_one 0;
    current_balance_test "current_balance is 500" state_one 500;
    change_balance_test "deduct $200 from original balance: $500" state_one
      (-200) 300;
    check_name "deduct $200 from original balance: $500" state_one (-200) 300
      "Prakriti";
    change_balance_test "add $200 to original balance: $500" state_one (-200)
      300;
    change_balance_test "add $0 to original balance: $500" state_one 0 500;
    make_owns_test "player with no properties, space 1, game" state_one 1
      game_board false;
      (*following test checks buy_property*)
    make_owns_test "player with owns = [1], space 1, game" state_two 1
      game_board true;
      (*following test checks buy_property*)
    make_owns_test "player with owns=[1], space 2, game" state_two 2 game_board
      false;
    (*following test checks change_owns*)
    make_owns_test "player with owns = [1]" state_three 1 game_board true;
    (**following test checks go*)
    current_pos_test "current pos of state_one after it has moved 2 steps is 2" go_state 2;
    (**following tests check pay_rent*)
    current_balance_test "current balance of rent_play1 is 496" rent_play1 496; 
    current_balance_test "current balance of rent_play2 is 444" rent_play2 444;
    current_balance_test "current balance of player_two is 440" player_two_insuf_funds 40;
    buy_property_exception_test "insufficient funds to buy property" player_two_insuf_funds 1 game_board
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ monopoly_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
