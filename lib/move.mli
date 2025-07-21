open PType

(** AF: A record named t.name which has the follwoing properties:
    - t.name is the name of the move
    - t. mtype: the pType associated with the move
    - t.pow: the power associated with the move
    - t.acc: the accuracy associated with the move
    - t.status_effect status effects associated with the move

    RI :
    - t.mtype is a pType object
    - strings of t.name are all lowercase
    - t.pow and t.acc cannot be negative
    - t.status effect can only be 0,1,or 3 *)

type t = {
  name : string;
  mtype : PType.t;
  pow : int;
  acc : int;
  status_effect : int option;
      (* 0 = no effect; 1 = sleep; 2 = poison; 3 = paralysis *)
}

val create_move : string -> PType.t -> int -> int -> t
(** [create_move name ty power acc] builds a move with an optional status effect
    . *)

val get_name : t -> string
(** Returns the move’s name. *)

val get_mtype : t -> string
(** Returns the move’s type as a string. *)

val get_power : t -> int
(** Returns the move’s base power. *)

val get_accuracy : t -> int
(** Returns the move’s accuracy. *)

val get_status_effect : t -> int option
(** Returns [Some n] if the move inflicts status code [n], else [None]. *)

val get_all_moves : t array
(** Array of every move, each carrying its own optional status effect. *)
