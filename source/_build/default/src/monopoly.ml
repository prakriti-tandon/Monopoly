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
  owner : string;
}

type space_type =
  | Go of {
      id : string;
      salary : int;
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
    owner = json |> member "owner" |> to_string;
  }

let info_of_json json =
  let stype = json |> member "type" |> to_string in
  let json = json |> member "info" in
  if stype = "go" then
    Go
      {
        id = json |> member "id" |> to_string;
        salary = json |> member "salary" |> to_int;
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

let owner mon property =
  match (find_space mon property).info with
  | Go a -> raise SpaceNotOwnable
  | Property b -> if b.owner = "" then None else Some b.owner

let replace_space mon prop newspace =
  let less_spaces = List.filter (fun x -> x.space_number < prop) mon.spaces in
  let more_spaces = List.filter (fun x -> x.space_number > prop) mon.spaces in
  {
    spaces = less_spaces @ (newspace :: more_spaces);
    num_spaces = mon.num_spaces;
  }

let set_owner mon property player =
  match (find_space mon property).info with
  | Go a -> raise SpaceNotOwnable
  | Property b ->
      let space_info = b in
      let new_space =
        {
          space_number = property;
          info = Property { space_info with owner = player };
        }
      in
      replace_space mon property new_space

let name mon property =
  match (find_space mon property).info with
  | Go a -> a.id
  | Property b -> b.id

let description mon property =
  match (find_space mon property).info with
  | Go a -> raise DescriptionNotAvailable
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