open PType
open Move

(** A single attack move with base power and accuracy. *)
type move = {
  name     : string;
  mtype    : PType.t;
  power    : int;
  accuracy : int;
}

(** A Pokémon with stats, type, and its moveset. *)
type t = {
  name: string;
  attack: int;
  hp: int;
  max_hp: int;
  xp: int;
  pType: PType.t;
  moves: Move.t array;
  status: int;  (* 0 = normal, 1 = sleep, 2 = poison, 3 = paralysis *)
}



(** Create a new Pokémon. *)
let create name attack hp xp pType moves =
  { name; attack; hp; max_hp = hp; xp; pType; moves; status = 0 }

let get_name    p = p.name
let get_attack  p = p.attack
let get_hp      p = p.hp
let get_max_hp  p = p.max_hp
let get_xp      p = p.xp
let get_ptype    p = p.pType
let get_moves   p = p.moves

let get_status p = p.status

let set_status p new_status = { p with status = new_status }

let gain_xp p gain = { p with xp = p.xp + gain }
(** Given a base‑power, compute accuracy:
    10 ⇒ 100%; each +5 power ⇒ –5% (floored at 0). *)
let calculate_accuracy power =
  let diff       = power - 10 in
  let decrements = diff / 5 in
  let acc        = 100 - (decrements * 5) in
  if acc < 0 then 0 else acc

(** Apply damage to a Pokémon, returning a new Pokémon with updated HP. **)
let take_damage p dmg =
  let new_hp = max 0 (p.hp - dmg) in
  { p with hp = new_hp }

(** True if [attacker]’s type is in [defender]’s weakness list. **)
let is_weak defender attacker =
  let def_type = get_ptype defender in
  let atk_type = get_type (get_ptype attacker) in
  List.mem atk_type (get_weak def_type)

(** True if [attacker]’s type is in [defender]’s resistance list. **)
let is_resistant defender attacker =
  let def_type = get_ptype defender in
  let atk_type = get_type (get_ptype attacker) in
  List.mem atk_type (get_resistant def_type)


(** Starter Pokémon movesets **)
let moveArray = Move.get_all_moves
let charmander_moves = [|moveArray.(0); moveArray.(1); moveArray.(2);|]
let squirtle_moves = [| moveArray.(3); moveArray.(4); moveArray.(5);|]
let bulbasaur_moves = [|moveArray.(3); moveArray.(6); moveArray.(2);|]
let dratini_moves = [| moveArray.(7); moveArray.(8); moveArray.(9); |]
let pikachu_moves = [| moveArray.(10); moveArray.(11); moveArray.(12); |]
let pidgey_moves = [| moveArray.(10); moveArray.(13); moveArray.(14); |]
let rattata_moves = [| moveArray.(10); moveArray.(12); moveArray.(15); |]
let machop_moves = [| moveArray.(16); moveArray.(17); moveArray.(18); |]
let gastly_moves = [|moveArray.(19); moveArray.(20); moveArray.(21);|]
let diglett_moves = [|moveArray.(0); moveArray.(22); moveArray.(23);|]
let geodude_moves = [| moveArray.(3); moveArray.(24); moveArray.(25); |]
let is_disadvantaged ~move ~defender =
  let move_type = Move.get_mtype move in
  let def_type = get_ptype defender in
  List.mem move_type (get_weak def_type)
