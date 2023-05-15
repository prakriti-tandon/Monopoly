(** Module that carries out the instructions on chance cards.

    This module performs the actions required by each chance card in the chance
    deck. *)

(**********************************************************************)

val exec_card :
  Deck.t ->
  Multiplayer.player_list ->
  Bank.t ->
  Board.t ->
  int ->
  State.t ->
  unit
(** [exec_card deck pls bank board i current_player] mutates all player states
    [pls] and the bank [bank] after player [current_player] draws chance card
    [i] from deck [deck]. *)
