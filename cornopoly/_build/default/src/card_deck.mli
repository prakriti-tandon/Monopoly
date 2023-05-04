(** Representation of card deck data.

    This module represents the deck of chance cards and the deck of community
    chest cards. The module reads in jsons for each deck of cards and provides
    functions that give the name and description of each card. *)

(**********************************************************************)

type t
(** The abstract type that represents a deck of cards in Monopoly.*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] initializes a deck of cards represented by json string [j].
    Requires: j must be a valid json representation of chance/community chest
    cards.*)

val name : int -> t -> string
(** [name c deck] is the name of card [c] in deck [deck].*)

val description : int -> t -> string
(** [description c deck] is the description of card [c] in deck [deck]*)
