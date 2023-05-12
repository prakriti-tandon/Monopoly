open Yojson.Basic.Util

type comm_chest_type =
  | Earn of int
  | Pay of int
  | Other

type card_info =
  | Chance of { newspace : int }
  | Comm of { ctype : comm_chest_type }

type card = {
  id : int;
  name : string;
  description : string;
  info : card_info;
}

type t = {
  cards : card list;
  num_cards : int;
}

let info_of_json json =
  let type_decider = json |> member "type" |> to_string_option in
  match type_decider with
  | None -> Chance { newspace = json |> member "newspace" |> to_int }
  | Some s ->
      let amt = json |> member "amt" |> to_int in
      Comm
        {
          ctype =
            (match s with
            | "earn" -> Earn amt
            | "pay" -> Pay amt
            | "other" -> Other
            | _ -> failwith "failure in creating comm chest card");
        }

let card_of_json json =
  {
    id = json |> member "card-id" |> to_int;
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    info = json |> info_of_json;
  }

let from_json json =
  {
    cards = json |> member "cards" |> to_list |> List.map card_of_json;
    num_cards = json |> member "num-cards" |> to_int;
  }

let find_card deck i =
  match List.filter (fun x -> x.id = i) deck.cards with
  | [ cd ] -> cd
  | _ -> failwith "Unknown card"

let name deck i = (find_card deck i).name
let description deck i = (find_card deck i).description
let number_cards deck = deck.num_cards
let random_card deck = failwith "Unimplemented"
