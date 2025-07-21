type t
(** AF: A Pokémon type named t.pType, which has the following properties:
    - t.weakTo is a list of type names representing the types this type is weak
      against.
    - t.resistantTo is a list of type names representing the types this type is
      resistant to.

    For example:
    - If t.pType = "fire", t.weakTo = ["water"; "rock"; "ground"] and
      t.resistantTo = ["grass"; "bug"; "steel"; "fire"], then AF(t) represents
      the Fire type, which is weak to Water, Rock, and Ground, but resistant to
      Grass, Bug, Steel, and Fire itself.

    RI :
    - t.pType is a non-empty string representing the name of the type.
    - All strings in t.weakTo and t.resistantTo are lowercase to ensure
      consistent type matching.
    - t.weakTo and t.resistantTo do not contain duplicate values.
    - A type cannot be both weak and resistant to the same type.

    Enforced via:
    - Lowercasing all type names when creating them.
    - Filtering duplicate entries in weakTo and resistantTo.
    - Ensuring intersection of weakTo and resistantTo is empty. *)

val create_type : string -> t
(** Creates a type with predefined weaknesses, resistances, and immunities. *)

val get_type : t -> string
(** Returns the type name. *)

val get_weak : t -> string list
(** Returns a list of types this type is weak to. *)

val get_resistant : t -> string list
(** Returns a list of types this type is resistant to. *)
