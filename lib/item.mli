type t = {
  name : string;
  heal : int;
}
(** AF: A record named t.name which has the follwoing properties:
    - t.name is the name of the move
    - t.heal is the healinng value associated with the item

    RI :
    - t.name is a string
    - t.heal is a non negative int *)

val def_items : t array
(** Default items. Can append these onto an Item.t array *)

val add_item : int -> t array -> t array
(** adds an item from items with the specified index to an Item.t array. Raises
    InvalidArgument if 0 < idx < Array.length items or *)
