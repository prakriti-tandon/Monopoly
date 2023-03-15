(** This module represents the state of a player of Monopoly. It includes the
    player's name, current position, the money they have and the properties they
    own*)

type t
(** The abstract type of values representing the player state. *)

val init_state : string -> t
(**[init_state str] is the initial state of the player with name [str]. In that
   state, the player's current position is the go position. They have 500
   dollars and no owned properties*)

val name : t -> string
(**[name player] is the name of the player represented by state [player]*)

val current_pos : t -> int
(** [current_pos player] is the current position of the state [player]*)

val change_owns : int -> t -> t
(**[change_owns pos player] is the new state of state [player] after property
   [pos] has been added to its set of owned properties*)

val go : t -> Monopoly.t -> t
(**[go player] is the new state of the [player] after they have rolled the dice
   and moved ahead in the game. [go player] only affects the current position of
   the [player] *)
