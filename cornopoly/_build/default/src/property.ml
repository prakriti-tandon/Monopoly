type status =
  | OwnedByOtherPlayer of State.t
  | NotOwned
  | OwnedByThisPlayer

(*player_list represents the list of all players*)
type player_list = State.t array

(*type combined_state is the type for a value that shows the combined state of
  the bank and a player *)
type combined_state = {
  player : State.t;
  bank : Bank.t;
}

exception InsufficientFunds

(*[propert_status pls property] determined the status of the property space
  [property] with the players [pls]*)
let property_status = raise (Failure "Unimp")

(*[determine_rent owner property board] determines the rent payable for
  [property] from the [board] and depending on the number of houses owned by
  [owner] of [property]. Precondition: [owner] owns [property]*)
let determine_rent = raise (Failure "Unimp")

(*[determine_price owner property board] determines the total cost of [property]
  from the [board] and depending on the number of houses owned by [owner] of
  [property]. Precondition: [owner] owns [property]*)
let determine_price = raise (Failure "Unimp")

(*[pay_rent pls curr_pl board] finds out the current position of [curr_pl],
  checks if a player in [player_list] owns it, conducts the transaction between
  the two players, and gives the new list of players with updated balances. If
  nobody owns the property, it gives the same [pls] list of players.
  InsufficientFunds] if the player does not have money.*)
let pay_rent = raise (Failure "Unimp")

(*[buy_property curr_pl board bank] makes the [curr_pl] buy its current
  position, and deposit the money in [bank]. It gives the combined resulting
  state of the [curr_pl] and [bank].T hrows InsufficientFunds] if the player
  does not have money.*)
let buy_property = raise (Failure "Unimp")

(*[build_house curr_pl board bank] build a house at the current position of
  [curr_pl] and conducts the appropriate transaction from the [curr_pl] to bank.
  It gives the combined resulting state of the [curr_pl] and [bank]. Throws
  [InsufficientFunds] if the player does not have money. *)
let build_house = raise (Failure "Unimp")

(*[sell_property curr_pl property board bank] allows [curr_pl] to sell property
  [property] to the [bank] and take the money equal to the price of the
  [property]*)
let sell_property = raise (Failure "Unimp")
