open Yojson.Basic.Util

exception UnknownSpace
exception SpaceNotOwnable
exception DescriptionNotAvailable
exception NoSalary

type property = {
  id : string;
  description : string;
  price : int;
  rent : int;
  price_per_house : int;
  rent_per_house : int;
}

type other = {
  id : string;
  description : string;
}

type space_type =
  | Go of {
      id : string;
      salary : int;
      description : string;
    }
  | Property of property
  | CommChest of other
  | Chance of other
  | Jail of other

type space = {
  space_number : int;
  info : space_type;
}

type t = {
  spaces : space list;
  num_spaces : int;
}

let property_of_json json =
  {
    id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
    price = json |> member "price" |> to_int;
    rent = json |> member "rent" |> to_int;
    price_per_house = json |> member "pph" |> to_int;
    rent_per_house = json |> member "rph" |> to_int;
  }

let other_of_json json =
  {
    id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
  }

let info_of_json json =
  let stype = json |> member "type" |> to_string in
  let json = json |> member "info" in
  match stype with
  | "go" ->
      Go
        {
          id = json |> member "id" |> to_string;
          salary = json |> member "salary" |> to_int;
          description = json |> member "description" |> to_string;
        }
  | "property" -> Property (property_of_json json)
  | "comm-chest" -> CommChest (other_of_json json)
  | "chance" -> Chance (other_of_json json)
  | "jail" -> Jail (other_of_json json)
  | _ -> failwith "Not a valid space type in json"

let space_of_json json =
  { space_number = json |> member "space" |> to_int; info = info_of_json json }

let from_json json =
  {
    spaces = json |> member "spaces" |> to_list |> List.map space_of_json;
    num_spaces = json |> member "number_spaces" |> to_int;
  }

let find_space mon property =
  match List.filter (fun x -> x.space_number = property) mon.spaces with
  | [ sp ] -> sp
  | _ -> raise UnknownSpace

let name mon property =
  match (find_space mon property).info with
  | Go a -> a.id
  | Property a -> a.id
  | Chance a -> a.id
  | CommChest a -> a.id
  | Jail a -> a.id

let description mon property =
  match (find_space mon property).info with
  | Go a -> a.description
  | Property a -> a.description
  | Chance a -> a.description
  | CommChest a -> a.description
  | Jail a -> a.description

let price mon property =
  match (find_space mon property).info with
  | Property b -> b.price
  | _ -> raise SpaceNotOwnable

let price_per_house mon property =
  match (find_space mon property).info with
  | Property b -> b.price_per_house
  | _ -> raise SpaceNotOwnable

let rent mon property =
  match (find_space mon property).info with
  | Property b -> b.rent
  | _ -> raise SpaceNotOwnable

let rent_per_house mon property =
  match (find_space mon property).info with
  | Property b -> b.rent_per_house
  | _ -> raise SpaceNotOwnable

let salary mon s =
  match (find_space mon s).info with
  | Go a -> a.salary
  | _ -> raise NoSalary

let space_type mon property =
  match (find_space mon property).info with
  | Go a -> "go"
  | Property b -> "property"
  | Chance c -> "chance"
  | CommChest d -> "comm-chest"
  | Jail e -> "jail"

let number_of_spaces game = game.num_spaces
