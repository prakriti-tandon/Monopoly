type t = int ref

exception InsufBankFunds

let init_bank (x : int) : t = ref x
let funds bank : int = !bank
let add_funds bank x = bank := !bank + x

let deduct_funds bank x =
  if x > !bank then raise InsufBankFunds else bank := !bank - x
