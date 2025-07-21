type t = {
  pType : string;
  weakTo : string list;
  resistantTo : string list;
}

let create_type pType =
  let weakTo, resistantTo =
    match String.lowercase_ascii pType with
    | "fire" ->
        ([ "water"; "ground"; "rock" ], [ "grass"; "bug"; "steel"; "fire" ])
    | "water" ->
        ([ "electric"; "grass"; "dragon" ], [ "fire"; "water"; "ice"; "steel" ])
    | "grass" ->
        ( [ "fire"; "flying"; "bug"; "ice"; "poison" ],
          [ "water"; "ground"; "grass"; "electric" ] )
    | "electric" -> ([ "ground" ], [ "electric"; "flying"; "steel" ])
    | "flying" -> ([ "electric"; "rock"; "ice" ], [ "grass"; "fighting"; "bug" ])
    | "normal" -> ([ "fighting" ], [])
    | "fighting" -> ([ "flying"; "psychic"; "fairy" ], [ "rock"; "bug"; "dark" ])
    | "ghost" -> ([ "ghost"; "dark" ], [ "poison"; "bug" ])
    | "ground" -> ([ "water"; "grass"; "ice" ], [ "poison"; "rock"; "electric" ])
    | "rock" ->
        ( [ "water"; "grass"; "fighting"; "ground"; "steel" ],
          [ "normal"; "fire"; "poison"; "flying" ] )
    | "dragon" ->
        ([ "ice"; "dragon"; "fairy" ], [ "fire"; "water"; "grass"; "electric" ])
    | _ -> ([], [])
  in
  { pType; weakTo; resistantTo }

let get_type t = t.pType
let get_weak t = t.weakTo
let get_resistant t = t.resistantTo
