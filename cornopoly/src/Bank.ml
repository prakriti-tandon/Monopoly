type t = int ref

exception InsufBankFunds

let init_state (x : int) : t = ref x
let add_funds bank x = bank := !bank + x

let deduct_funds bank x =
  if x < !bank then raise InsufBankFunds else bank := !bank - x
