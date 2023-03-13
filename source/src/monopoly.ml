type property = {
  id : string;
  price : int;
  rent : int;
  owner : string;
}

type space_type =
  | Go of { salary : int }
  | Property of property

type space = {
  space_number : int;
  info : space_type;
}

type t = {
  spaces : space list;
  num_spaces : int;
}

let from_json json = raise (Failure "Unimplemented: Monopoly.from_json")
let owner player = raise (Failure "Unimplemented: Monopoly.owner")

let set_owner property player mon =
  raise (Failure "Unimplemented: Monopoly.set_owner")

let name property = raise (Failure "Unimplemented: Monopoly.name")
let description property = raise (Failure "Unimplemented: Monopoly.description")
let price property = raise (Failure "Unimplemented: Monopoly.price")
let rent property = raise (Failure "Unimplemented: Monopoly.rent")
let space_type property = raise (Failure "Unimplemented: Monopoly.type")
