type command =
  | Go
  | Number_of_players of int
  | Player_name of string
  | Yes
  | No
  | Quit
  | Draw

exception Empty

let parse str =
  match str with
  | "" -> raise Empty
  | "go" -> Go
  | "yes" -> Yes
  | "no" -> No
  | "quit" -> Quit
  | "draw" -> Draw
  | t -> (
      match int_of_string_opt t with
      | Some i -> Number_of_players i
      | None -> Player_name t)
