type t = unit

(*expception InsufficientFunds is thrown when the bank runs out of money*)
exception InsufficientFunds

(*[add_funds bank a] adds [a] amount of funds to [bank]. This function is used
  when buying a property, or repaying a loan, or any action that increases the
  funds in the bank *)
let add_funds x y = raise (Failure "Unimplemented")

(*[deduct_funds bank a] removes [a] amount of funds from [bank]. This function
  is called when taking a loan from the bank. It throws [InsufficientFunds] if
  the bank has no money left*)
let deduct_funds x y = raise (Failure "Unimplemented")
