let exec_go pls curr_player board =
  let sal = Board.salary board 0 in
  let () =
    Property.update_player pls curr_player
      (State.change_balance curr_player sal)
  in
  let () =
    let check_owes x =
      match State.owes_to_bank x with
      | Some x, y ->
          Property.update_player pls curr_player
            (State.turn_in_debt curr_player 1)
      | None, _ -> ()
    in
    check_owes curr_player
  in
  ()
