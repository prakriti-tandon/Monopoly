(** Representation of monopoly board data.

    This module represents a monopoly board with information about each space
    such as property name, price, rent price, and its owner (if any). *)

(**********************************************************************)

type t
(** The abstract type that represents the monopoly board. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] initializes the monopoly board from file [j]. *)

val owner : int -> string
(** [owner p] is the name of the player that owns the property at space [p] or
    None if no player owns property [p]. *)

val set_owner : int -> string -> t -> t
(** [owner p u m] is a the monopoly board [m] such that player [u] owns the
    property at space [p]. *)

val name : int -> string
(** [description p] is the name of the property or other space at space [p]. *)

val description : int -> string
(** [description p] is the description of the property at space [p]. *)

val price : int -> int
(** [price p] is the price to purchase the property at space [p]. *)

val rent : int -> int
(** [rent p] is the rent price for the property at space [p]. *)

val space_type : int -> string
(** [space_type p] is the type of space p. As of MS1, this can either be
    "property" or "go". *)

val number_of_spaces : t -> int
(**[number_of_spaces game] is the number of spaces in monopoly [game]*)
