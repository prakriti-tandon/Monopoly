let exec_go pls curr_player board =
  let do_salary pls curr_player board =
    let sal = Board.salary board 0 in
    Property.update_player pls curr_player
      (State.change_balance curr_player sal)
  in
  let check_owes z =
    match State.owes_to_bank z with
    | Some x, y -> Property.update_player pls z (State.turn_in_debt z 1)
    | None, _ -> ()
  in
  do_salary pls curr_player board;
  check_owes pls.(Property.find_index (State.name curr_player) pls)
