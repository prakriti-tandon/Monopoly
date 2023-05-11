(** Representation of monopoly board data.

    This module represents a monopoly board with information about each space
    such as property name, price, rent price, and its owner (if any). *)

(**********************************************************************)

type t
(** The abstract type that represents the monopoly board. *)

exception UnknownSpace
(** Raised when a space indicated by its integer position cannot be found. *)

exception SpaceNotOwnable
(** Raised when non-property spaces are queried for property-like attributes. *)

exception NoSalary
(** Raised when a user queries for the salary of a space that does not provide
    one.*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] initializes the monopoly board from file [j]. *)

val name : t -> int -> string
(** [name m s] is the name of the property or other space at space [s] in
    monopoly board [m]. *)

val description : t -> int -> string
(** [description m s] is the description of space [s] in monopoly board [m]. *)

val price : t -> int -> int
(** [price m s] is the price to purchase the property at space [s] in monopoly
    board [m]. Raises [SpaceNotOwnable] if [s] is not a property. *)

val price_per_house : t -> int -> int
(** [price_per_house m s] is the price to purchase a house for the property at
    space [s] in monopoly board [m]. Raises [SpaceNotOwnable] if [s] is not a
    property. *)

val price_per_hotel : t -> int -> int
(** [price_per_hotel m s] is the price to purchase a hotel for the property at
    space [s] in monopoly board [m]. Raises [SpaceNotOwnable] if [s] is not a
    property. *)

val rent : t -> int -> int
(** [rent m s] is the rent price for the property at space [s] in monopoly board
    [m]. Raises [SpaceNotOwnable] if [s] is not a property. *)

val rent_per_house : t -> int -> int
(** [rent_per_house m s] is the rent price per house for the property at space
    [s] in monopoly board [m]. Raises [SpaceNotOwnable] if [s] is not a
    property. *)

val rent_per_hotel : t -> int -> int
(** [rent_per_hotel m s] is the rent price per hotel for the property at space
    [s] in monopoly board [m]. Raises [SpaceNotOwnable] if [s] is not a
    property. *)

val salary : t -> int -> int
(** [salary m s] is the salary players acquire by passing space [s] Raises
    [NoSalary] if space [s] does not give the player a salary.*)

val space_type : t -> int -> string
(** [space_type m s] is the type of space p in monopoly board [s]. May evaluate
    to "go", "property", "chance", "comm-chest", or "jail". *)

val number_of_spaces : t -> int
(**[number_of_spaces m] is the number of spaces in board [m]*)

val between_spaces : t -> int -> int -> int
(** [between_spaces m sp1 sp2] is the number of spaces a player must travel
    forward to land on space [sp2] when they are currently on [sp1] in monopoly
    board [m].*)
