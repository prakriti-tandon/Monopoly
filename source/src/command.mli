(**The type [command] represents a player command.*)
type command =
  | Go
  | Number_of_players of int
  | Player_name of string
  | Yes
  | No

exception Empty
(** Raised when an empty command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows.

    - [parse "go"] is [Go]
    - [parse "yes"] is [Yes]
    - [parse "no"] is [No]
    - [parse "Prakriti"] is [Player_name "Prakriti"]
    - [parse "2"] is [Number_of_players 2]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9); no spaces, tabs
    or newlines, etc. Input value for player name can't be an integer).

    Raises: [Empty] if [str] is the empty string. *)
