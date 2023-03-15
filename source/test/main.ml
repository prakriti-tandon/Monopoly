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

let board = Monopoly.from_json (Yojson.Basic.from_file "data/board.json")

let owner_test (name : string) (board : Monopoly.t) (space : int)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Monopoly.owner board space)

let monopoly_tests = []

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
