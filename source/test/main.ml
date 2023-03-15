open OUnit2
open Game
open Monopoly
open Command
open State
open Yojson

let board = Monopoly.from_json (Yojson.Basic.from_file "data/board.json")

let owner_test (name : string) (board : Monopoly.t) (space : int)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Monopoly.owner board space)

let monopoly_tests = []
let command_tests = []
let state_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten [ monopoly_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
