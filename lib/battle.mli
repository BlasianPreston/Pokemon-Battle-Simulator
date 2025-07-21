(* lib/battle.mli *)

open Pokemon

type t = {
  player : Pokemon.t array;
  opponent : Pokemon.t array;
  items : Item.t array;
}
(** AF: A record named t.name which has the follwoing properties:
    - t.player is the array of pokemon associated with the player
    - t.opponent is the array of pokemon associated with the A.I. opponent
    - t.items are the items that can be used by the player

    RI :
    - t.player and t.opponent are both arrays of pokemon objects with at least
      one pokemon in them
    - t.item is an array of t.item objects *)

val init_battle : Pokemon.t -> Pokemon.t -> t
(** Initialize a 1‑vs‑1 battle. *)

val poke_name : Pokemon.t array -> string
(** Query the first slot’s name or HP. *)

val poke_hp : Pokemon.t array -> string

val get_player : t -> Pokemon.t array
(** Accessors & team management. *)

val get_opponent : t -> Pokemon.t array
val add_pokemon : t -> on_player:bool -> Pokemon.t -> t
val switch : t -> on_player:bool -> int -> unit
val check_faint : t -> on_player:bool -> int -> bool

val take_turn : t -> Move.t -> player_turn:bool -> unit
(** Perform one turn using the given move:
    - rolls against [get_accuracy move],
    - computes base damage = attacker.attack * move.power / 10,
    - applies ×2/½ on weakness/resistance,
    - updates defender’s HP. **)

val heal : Pokemon.t -> int -> Pokemon.t

val use_item : t -> Pokemon.t -> int -> t
(** Uses item from [b.items] at [item_idx] to heal the given pokemon and remove
    item from inventory. Returns as a [Battle.t] where battle no longer has that
    item and player.(0) is healed *)
