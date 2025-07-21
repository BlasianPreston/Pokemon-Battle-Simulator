open PType
open Move
(** AF: A Pokémon named t.name which has the follwoing properties:
    - t.name is the name of the pokemon
    - t.attack is the attack value of the pokemon
    -t.hp is the current hp of the pokemon, the amount of damage they can take before fainitng at the current point in time
    -t.max_hp is the hp of the pokemon having not yet been damaged, is the amount of damage points they can take total before fainting
    -t.xp is the amount of xp a pokemon has, if a pokemon gains enough xp it may evolve
    -t.PType is the type object associated with the pokemon containing a type name, a weakness list, and a resistance list
    -t.moves is the set of moves a given pokemon has access to, a pokemon may have up to 4 different moves

    RI :
    - t.pType is a an object of pType a record containing a string t.pType and two lists t.resistances and t.weakneses
    - strings of t.name are all lowercase
    - t.moves does not contain duplicate moves

    *)


(** A Pokémon with stats, type, and its moveset. *)
type t = {
  name   : string;
  attack : int;
  hp     : int;
  max_hp : int;
  xp     : int;
  pType  : PType.t;
  moves  : Move.t array;
  status : int;
}

(** Create a new Pokémon. *)
val create :
  string      (* name *) ->
  int         (* attack *) ->
  int         (* hp *) ->
  int         (* xp *) ->
  PType.t     (* primary type *) ->
  Move.t array  (* moveset *) ->
  t

val get_name    : t -> string
val get_attack  : t -> int
val get_hp      : t -> int
val get_max_hp  : t -> int
val get_xp      : t -> int
val get_ptype   : t -> PType.t
val get_moves   : t -> Move.t array
val get_status : t -> int
val set_status : t -> int -> t

val gain_xp     : t -> int -> t
(** Compute accuracy from base power (10→100%, +5 power ⇒ –5%). *)
val calculate_accuracy : int -> int

(** Hit‐point management. *)
val take_damage  : t -> int -> t

(** Type‐effectiveness predicates. *)
val is_weak      : t -> t -> bool
val is_resistant : t -> t -> bool

(** Predefined movesets for each Pokémon. *)
val charmander_moves : Move.t array
val squirtle_moves   : Move.t array
val bulbasaur_moves  : Move.t array
val dratini_moves    : Move.t array
val pikachu_moves    : Move.t array
val pidgey_moves     : Move.t array
val rattata_moves    : Move.t array
val machop_moves     : Move.t array
val gastly_moves     : Move.t array
val diglett_moves    : Move.t array
val geodude_moves    : Move.t array

val is_disadvantaged: move:Move.t -> defender:t -> bool