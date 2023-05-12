(*This module contains all the functions associated with changing the state of
  multiple players whenever they land on a property i.e. Multiplayer
  functionality*)

(*type status is [OwnedByOtherPlayer a] if the property is owned by player [a],
  [a] is not the player currently at this property. It is [NotOwned] if nobody
  owns it and it is [OwbedByThisPlayer] if the player who owns the property is
  also standing at the current position. *)
type status =
  | OwnedByOtherPlayer of State.t
  | NotOwned
  | OwnedByThisPlayer

(*player_list represents the list of all players*)
type player_list = State.t array

(*[InsufficientFunds] is raised when a player does not have sufficient funds for
  a transaction*)
exception InsufficientFunds

(*[SpaceNotOwnable] is raised when a rent or price feature is being determined
  for a non-property space*)
exception SpaceNotOwnable

(*[propert_status pls curr_pl] determined the status of the property space
  [curr_pl] is at with the players [pls]*)
val property_status : player_list -> State.t -> Board.t -> status

(*[determine_rent owner property board] determines the rent payable for
  [property] from the [board] and depending on the number of houses owned by
  [owner] of [property]. Throws [SpaceNotOwnable] if space cannot be owned.
  Precondition: [owner] owns [property]*)
val determine_rent : State.t -> int -> Board.t -> int

(*[find_index name pls] finds the index of the player in [pls] with the same
  name as [name]*)
val find_index : string -> player_list -> int

(*[determine_price owner property board] determines the total cost of [property]
  from the [board] and depending on the number of houses owned by [owner] of
  [property]. Throws [SpaceNotOwnable] if space cannot be owned. Precondition:
  [owner] owns [property]*)
val determine_price : State.t -> int -> Board.t -> int

(*[pay_rent pls curr_pl board] finds out the current position of [curr_pl],
  checks if a player in [pls] owns it, conducts the transaction between the two
  players, and mutates list of players with updated balances. If nobody owns the
  property, [pls] remains the same.Throws [InsufficientFunds] if the player does
  not have money.*)
val pay_rent : player_list -> State.t -> Board.t -> unit

(*[buy_property_from_player pls curr_pl board] finds out the current position of
  [curr_pl], checks if a player in [pls] owns it, conducts the transaction
  between the two players, and mutates the list of players with updated player
  states. If nobody owns the property [pls] remains the same. Throws
  [InsuffcientFunds] if the player does have money to buy it *)
val buy_property_from_player : player_list -> State.t -> Board.t -> unit

(*[update_player pls old new] updates the state of the old player [old] with its
  new state [new]. Precondition: [old] and [new] have the same name (i.e. same
  players but with different states)*)
val update_player : player_list -> State.t -> State.t -> unit
