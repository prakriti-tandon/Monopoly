(** Module that reads in the chance and community chest card decks.

    This module translates a json card deck to a card deck type t and has
    functions common to chance and community chest to get information from
    individual cards. *)

(**********************************************************************)

type t
(** The card deck type. *)

val from_json : Yojson.Basic.t -> bool -> t
(** [from_json j chance] initializes a card deck from a json file [j].
    Initializes a chance deck if [chance = true] and a community chest deck
    otherwise.*)

val name : t -> int -> string
(** [name deck i] is the name of card [i] in deck [deck]. *)

val description : t -> int -> string
(** [description deck i] is the description of card [i] in deck [deck]. *)

val random_card : t -> int
(** [random_card deck] is the integer ID of a random card from deck [deck].*)

val number_cards : t -> int
(** [number_cards deck] is the number of cards in deck [deck].*)
