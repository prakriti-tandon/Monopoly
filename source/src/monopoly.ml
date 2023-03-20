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
}

type space_type =
  | Go of {
      id : string;
      salary : int;
      description : string;
    }
  | Property of property

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
  }

let info_of_json json =
  let stype = json |> member "type" |> to_string in
  let json = json |> member "info" in
  if stype = "go" then
    Go
      {
        id = json |> member "id" |> to_string;
        salary = json |> member "salary" |> to_int;
        description = json |> member "description" |> to_string;
      }
  else Property (property_of_json json)

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
  | Property b -> b.id

let description mon property =
  match (find_space mon property).info with
  | Go a -> a.description
  | Property b -> b.description

let price mon property =
  match (find_space mon property).info with
  | Go a -> raise SpaceNotOwnable
  | Property b -> b.price

let rent mon property =
  match (find_space mon property).info with
  | Go a -> raise SpaceNotOwnable
  | Property b -> b.rent

let salary mon s =
  match (find_space mon s).info with
  | Go a -> a.salary
  | _ -> raise NoSalary

let space_type mon property =
  match (find_space mon property).info with
  | Go a -> "go"
  | Property b -> "property"

let number_of_spaces game = game.num_spaces
