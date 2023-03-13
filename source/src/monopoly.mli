(** Representation of monopoly board data.

    This module represents a monopoly board with information about each space
    such as property name, price, rent price, and its owner (if any). *)

(**********************************************************************)

type t
(** The abstract type that represents the monopoly board. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] initializes the monopoly board from file [j]. *)

val owner : string -> string
(** [owner p] is the name of the player that owns the property at space [p] or
    None if no player owns property [p]. *)

val set_owner : string -> string -> t -> t
(** [owner p u m] is a the monopoly board [m] such that player [u] owns the
    property at space [p]. *)

val description : string -> string
(** [description p] is the description of the property at space p. *)

val price : string -> int
(** [price p] is the price to purchase the property at space p. *)

val rent : string -> int
(** [rent p] is the rent price for the property at space p. *)
