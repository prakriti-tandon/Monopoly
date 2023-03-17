(** This module represents the state of a player of Monopoly. It includes the
    player's name, current position, the money they have and the properties they
    own*)

type t
(** The abstract type of values representing the player state. *)

type combined_state
(**The abstract type of the combined state of player1, player2, and monopoly
   game*)

(**The type represents the result of a buy or pay_rent action*)
type result =
  | Illegal
  | Legal of combined_state

val init_state : string -> t
(**[init_state str] is the initial state of the player with name [str]. In that
   state, the player's current position is the go position. They have 500
   dollars and no owned properties*)

val name : t -> string
(**[name player] is the name of the player represented by state [player]*)

val current_pos : t -> int
(** [current_pos player] is the current position of the state [player]*)

val current_balance : t -> int
(**[current_balance player] is the balance/amount of money in the player's bank
   account in their current state [player] *)

val change_balance : t -> int -> t
(**[change_balance player amt] is the new state of player's state [player] after
   their balance is changed by [amt]. If [amt] is negative, money will be
   deducted from the current balance in state [player] by [amt]. If [amt] is
   negative, that means money will be added to the current balance in state
   [player] by [amt]. *)

val owns : t -> string -> Monopoly.t -> bool
(**[owns player s game] is whether the [player] owns the property located at
   space [s] in the [game]. Raises [SpaceNotOwnable] if [s] is not a valid
   property in the [game]. *)

val change_owns : int -> t -> t
(**[change_owns pos player] is the new state of state [player] after property
   [pos] has been added to its set of owned properties*)

val go : t -> Monopoly.t -> t
(**[go player] is the new state of the [player] after they have rolled the dice
   and moved ahead in the game. [go player] only affects the current position of
   the [player] *)

val pay_rent : t -> t -> Monopoly.t -> result
(**[pay_rent player1 player2 game] is the result of [player1] paying rent to
   [player2] in the Monopoly [game]

   - If [player1] has insufficient balance, result is [Illegal]
   - Otherwise, result is a [Legal combined_result] with new player1' , new
     player2' and same game*)

val buy_property : t -> int -> Monopoly.t -> result
(**[buy_property player space game] is the result of [player1] buying a property
   located at [space] in the current [game]. -If the [player1] has insufficient
   funds, result is [Illegal] -If the [space] is not a valid [space] in the
   [game], result is [Illegal] -Otherwise, the result is [Legal combined_result]
   with new player' and a new game *)
