(**Bank represents the Bank in the Cornopoly. This is the entity that you as a
   player pay money to if you buy a property. it also allows for taking loans*)

(*type t represents the abstract type of a bank*)
type t

(*expception InsufficientFunds is thrown when the bank runs out of money*)
exception InsufficientFunds

(*[add_funds bank a] adds [a] amount of funds to [bank]. This function is used
  when buying a property, or repaying a loan, or any action that increases the
  funds in the bank *)
val add_funds : t -> int -> t

(*[deduct_funds bank a] removes [a] amount of funds from [bank]. This function
  is called when taking a loan from the bank. It throws [InsufficientFunds] if
  the bank has no money left*)
val deduct_funds : t -> int -> t
