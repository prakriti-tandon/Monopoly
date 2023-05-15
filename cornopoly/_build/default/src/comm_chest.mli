(** Module that carries out the instructions on community chest cards.

    This module performs the actions required by each community chest card in
    the community chest deck. *)

(**********************************************************************)

val exec_card :
  Deck.t ->
  Multiplayer.player_list ->
  Bank.t ->
  Board.t ->
  int ->
  State.t ->
  unit
(** [exec_card pls bank board player c] mutates the state of all players in
    player list [pls] after the current player [player] takes community chest
    card [c] from deck [deck], with current bank state [bank] and board [board]. *)
