type t = {
  name : string;
  heal : int;
}

let def_items =
  [|
    { name = "Potion"; heal = 20 };
    { name = "High Potion"; heal = 40 };
    { name = "Max Potion"; heal = 60 };
    { name = "Revive"; heal = 50 };
    { name = "Full Restore"; heal = 100 };
  |]

let add_item idx arr1 =
  Array.append arr1 (Array.make 1 (Array.get def_items idx))
