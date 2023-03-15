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

let owner_test = test_maker Monopoly.owner
let owner_excep_test = test_maker_exception Monopoly.owner SpaceNotOwnable

let set_owner_excep_test name board space player =
  name >:: fun _ ->
  assert_raises SpaceNotOwnable (fun () ->
      Monopoly.set_owner board space player)

let name_test = test_maker Monopoly.name
let description_test = test_maker Monopoly.description

let descrip_excep_test =
  test_maker_exception Monopoly.description DescriptionNotAvailable

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
    owner_test "Owner of CTB is None" board 14 None;
    (* Tests set_owner *)
    owner_test "Owner of CTB now Doug"
      (set_owner board 14 "doug")
      14 (Some "doug");
    owner_excep_test "Go has no owner" board 0;
    set_owner_excep_test "Can't give Go an owner" board 0 "doug";
    name_test "Space 0 named 'Go'" board 0 "Go";
    name_test "Space 1 named 'Balch Hall" board 1 "Balch Hall";
    description_test "Physical Sciences description" board 4
      "Home of the Goldie's chicken panini.";
    descrip_excep_test "Go has no desc." board 0;
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

let state_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten [ monopoly_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
