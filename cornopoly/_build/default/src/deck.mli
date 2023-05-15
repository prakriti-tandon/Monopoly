(** Module that reads in the chance and community chest card decks.

    This module translates a json card deck to a card deck type t and has
    functions common to chance and community chest to get information from
    individual cards. *)

(**********************************************************************)
exception IncorrectCardType
exception NoNewSpace

type t
(** The card deck type. *)

type comm_chest_type =
  | Earn of int option
  | Pay of int option
  | Other
      (** The type of community chest cards, where [Earn x] is a card which
          gives the player money, [Pay y] is a card that takes money away from
          the player, and [Other] is a unique card with some other function. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] initializes a card deck from a json file [j]*)

val name : t -> int -> string
(** [name deck i] is the name of card [i] in deck [deck]. *)

val description : t -> int -> string
(** [description deck i] is the description of card [i] in deck [deck]. *)

val random_card : t -> unit -> int
(** [random_card deck] is the integer ID of a random card from deck [deck].*)

val number_cards : t -> int
(** [number_cards deck] is the number of cards in deck [deck].*)

val new_space : t -> int -> int option
(** [new_space deck i] is Some new space that card [i] tells the player to move
    to in deck [deck] or None if the card does not prompt the player to move
    spaces. Raises: [IncorrectCardType] if passed a community chest card deck. *)

val comm_chest_info : t -> int -> comm_chest_type
(** [comm_chest_info deck i] gives the community chest type of card [i] in deck
    [deck]. Raises: [IncorrectCardType] if passed a chance card deck.*)
