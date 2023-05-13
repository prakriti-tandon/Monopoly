(** This module represents the state of a single player of Monopoly. It includes
    the player's name, current position, the money they have and the properties
    they own*)

type t
(** The abstract type of values representing the player state. *)

type property
(** The abstract type representing a property owned by the player and the number
    of houses and hotels the player owns at the property. *)

exception InsufficientFunds
(**Following exception is raised when the player has insufficient funds to buy
   property or pay rent*)

exception ExceededHouseLimit
(**Following excpeiton is raised when the [player1] is trying to buy too many
   houses (>=4) or buying a num of houses that will cause them to exceed the
   limit of 4,*)

exception ExceededHotelLimit
(**Following excpetion is raised when the [player1] is trying to buy too many
   hotels (>=2) or buying a num of hotels that will cause them to exceed the
   limit of 2 *)

exception DoesntOwnProperty

val init_state : string -> t
(**[init_state str] is the initial state of the player with name [str]. In that
   state, the player's current position is the go position. They have 500
   dollars, no owned properties, and no debt to bank.*)

val name : t -> string
(**[name player] is the name of the player represented by state [player]*)

val current_pos : t -> int
(** [current_pos player] is the current position of the state [player]*)

val owns_list : t -> property list
(** [owns_list player] is a list of properties owned by the state [player].*)

val make_property : int -> int -> int -> property
(** [make_property space num_houses num_hotels] is a property with a [space],
    [num_houses], and [num_hotels] *)

val num_houses : t -> int -> int
(**[num_houses player s] is the number of houses the player owns at space [s].
   Raises [DoesntOwnProperty] if the player doesnt own the property at space
   [s].*)

val num_hotels : t -> int -> int
(**[num_hotels player s game] is the number of hotels the player owns at space
   [s].Raises [DoesntOwnProperty] if the player doesnt own the property at space
   [s].*)

val current_balance : t -> int
(**[current_balance player] is the balance/amount of money in the player's bank
   account in their current state [player] *)

val owes_to_bank : t -> int option * int
(**[owes player] is a tuple (x,y) where x is amount of money the player owes the
   bank and y is the number of times they can pass go before they need to repay
   the loan. *)

val change_owes : t -> int -> t
(**[change_owes player amt] is the new state of player's state [player] after
   they pay off their loan by some [amt]. As a result, they owe less money to
   the bank. The [amt] paid off <= current amt player owes. *)

val jail : t -> int option
(*[jail player] is the number of turns they have to be in jail. -If they are not
  in jail, will return None option. -If they are in jail and have zero turns
  left in jail, they can be released. Return Some 0*)

val put_in_jail : t -> t
(**[put_in_jail player] is the new state of player's state [player] after they
   are put into jail. The number of turns they have to wait until they can get
   out of jail is initialized to Some 3. (Jail field is set to Some 3) *)

val get_out_of_jail : t -> t
(**[get_out_of_jail player] is the new state of player's state [player] after
   they escape jail. The jail field is set back to None.*)

val change_balance : t -> int -> t
(**[change_balance player amt] is the new state of player's state [player] after
   their balance is changed by [amt]. If [amt] is negative, money will be
   deducted from the current balance in state [player] by [amt]. If [amt] is
   positive, that means money will be added to the current balance in state
   [player] by [amt]. *)

val compare_property : property -> property -> int
(**[compare_property x y] compares the space numbers at property [x] and
   property [y]. Returns 1 if property [x]'s space number > property [y]'s space
   number. Returns -1 if property [x]'s space number < property [y] 's space
   number. Returns 0 if equal. This function is like Stdlib.compare but specific
   for dealing with properties. *)

val owns : t -> int -> Board.t -> bool
(**[owns player s game] is whether the [player] owns the property located at
   space [s] in the [game]. *)

val change_owns : int -> t -> t
(**[change_owns pos player] is the new state of state [player] after property
   [pos] has been added to its set of owned properties*)

val go : int -> t -> Board.t -> t
(**[go dice player] is the new state of the [player] after they have moved ahead
   [dice] spaces in the game. [go player] only affects the current position of
   the [player] *)

val dice : int
(** [dice] is a random integer generated between 0 and 6 (both inclusive). It is
    the result of a dice roll*)

val buy_property : t -> int -> Board.t -> Bank.t -> t
(**[buy_property player space game] is the result of [player1] buying a property
   located at [space] in the current [game].

   -If the [player1] has insufficient funds, result is [InsufficientFunds]
   \-Otherwise, the result is new player'*)

val buy_house : t -> int -> Board.t -> int -> Bank.t -> t
(**[buy_house player space game x bank] is the result of [player1] buying [x]
   number of houses (<=4) located at [space] in the current [game]. The maximum
   number of houses a given player can have is 4.

   -If the [player1] has insufficient funds, result is [InsufficientFunds] -If
   the [player1] is trying to buy too many houses (>=4) or buying a num of
   houses that will cause them to exceed the limit of 4, result is
   [ExceededHouseLimit] -Otherwise, the result is new player'*)

val add_hotel : t -> int -> Board.t -> t
(** [add_hotel player space game] adds a hotel to the property at [space] in
    monopoly board [game] that is owned by [player]. This function does not
    involve any financial transactions and only manipulates the player state.*)

val buy_hotel : t -> int -> Board.t -> int -> Bank.t -> t
(**[buy_hotel player space game x bank] is the result of [player1] buying [x]
   number of hotels (<=2) located at [space] in the current [game]. The maximum
   number of houses a given player can have is 2.

   -If the [player1] has insufficient funds, result is [InsufficientFunds] -If
   the [player1] is trying to buy too many hotels (>=2) or buying a num of
   hotels that will cause them to exceed the limit of 2, result is
   [ExceededHotelLimit] -Otherwise, the result is new player'*)

val sell_property : t -> int -> Board.t -> Bank.t -> t
(**[sell_property player space game x bank] is the result of [player1] selling a
   property located at [space] in the current [game]. *)

val sell_house : t -> int -> Board.t -> Bank.t -> t
(**[sell_house player space game x bank] is the result of [player1] selling a
   house located at [space] in the current [game]. *)

val sell_hotel : t -> int -> Board.t -> Bank.t -> t
(**[sell_hotel player space game x bank] is the result of [player1] selling a
   hotel located at [space] in the current [game]. *)

val space_of_property : property -> int
(** [space_of_property p] is the integer space number of property [p].*)

val remove_owns : int -> t -> t
(* Prakriti - [remove_owns space p] removes [space] from the set of owned
   properties by player [p]*)
