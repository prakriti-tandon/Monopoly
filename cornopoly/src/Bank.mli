(** Bank represents the Bank in the Cornopoly. This is the entity that you as a
    player pay money to if you buy a property. it also allows for taking loans*)

type t
(** type t represents the abstract type of a bank*)

exception InsufBankFunds
(** expception InsufBankFunds is thrown when the bank runs out of money*)

val init_bank : int -> t
(** create a bank*)

val add_funds : t -> int -> unit
(** [add_funds bank a] adds [a] amount of funds to [bank]. This function is used
    when buying a property, or repaying a loan, or any action that increases the
    funds in the bank *)

val deduct_funds : t -> int -> unit
(** [deduct_funds bank a] removes [a] amount of funds from [bank]. This function
    is called when taking a loan from the bank. It throws [InsufBankFunds] if
    [a] is less than the money in bank*)
