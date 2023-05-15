(** Handles players passing the space "Go"

    This module performs the necessary changes to player states that occur when
    a player passes the "Go" space. *)

(**********************************************************************)

val exec_go : Multiplayer.player_list -> State.t -> Board.t -> unit
(** [exec_go pls current_player board] updates the state of player
    [current_player] within player list [pls] when they pass the "Go" space in
    board [board]. *)
