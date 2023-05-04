(**Bank represents the Bank in the Cornopoly. This is the entity that you as a
   player pay money to if you buy a property. it also allows for taking loans*)

type t

exception InsufficientFunds

val add_funds : t -> int -> t
val deduct_funds : t -> int -> t
