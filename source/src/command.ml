type command =
  | Go
  | Number_of_players of int
  | Player_name of string
  | Yes
  | No

exception Empty

let parse = raise (Failure "Unimplemented command.parse")
