(*This module contains all the funcctions associated with changing the state of
  a player or players whenever they land on a property*)

type status =
  | OwnedByOtherPlayer of State.t
  | NotOwned
  | OwnedByThisPlayer
