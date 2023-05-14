let exec_go pls curr_player board =
  let sal = Board.salary board 0 in
  Property.update_player pls curr_player (State.change_balance curr_player sal)
