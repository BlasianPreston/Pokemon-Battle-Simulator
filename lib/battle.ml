open Pokemon
open PType
open Random
open Move
open Item

type t = {
  player : Pokemon.t array;
  opponent : Pokemon.t array;
  items : Item.t array;
}

(* ------------------------------------------------------------ *)
let () = Random.self_init ()

let init_battle pk1 pk2 =
  {
    player = [| pk1 |];
    opponent = [| pk2 |];
    items = Array.make 1 Item.def_items.(0);
  }

let poke_name team = Pokemon.get_name team.(0)
let poke_hp team = Pokemon.get_hp team.(0) |> string_of_int
let get_player b = b.player
let get_opponent b = b.opponent
let get_items b = b.items

let add_pokemon b ~on_player mon =
  if on_player then { b with player = Array.append b.player [| mon |] }
  else { b with opponent = Array.append b.opponent [| mon |] }

let switch b ~on_player idx =
  let arr = if on_player then b.player else b.opponent in
  if idx < 0 || idx >= Array.length arr then invalid_arg "index out of bounds";
  if idx <> 0 then (
    let tmp = arr.(0) in
    arr.(0) <- arr.(idx);
    arr.(idx) <- tmp)

let check_faint b ~on_player idx =
  let arr = if on_player then b.player else b.opponent in
  if idx < 0 || idx >= Array.length arr then invalid_arg "index out of bounds";
  Pokemon.get_hp arr.(idx) = 0

(* ------------------------------------------------------------ *)
(* helper : if the active Pokémon on [team] has fainted, swap in
            the first healthy reserve if one exists *)
let handle_faint team =
  if Pokemon.get_hp team.(0) > 0 then ()
  else
    let rec find i =
      if i >= Array.length team then () (* no healthy mons left *)
      else if Pokemon.get_hp team.(i) > 0 then (
        let tmp = team.(0) in
        team.(0) <- team.(i);
        team.(i) <- tmp)
      else find (i + 1)
    in
    find 1

(* Move ⇒ optional status it inflicts *)
let status_of_move mv = Move.get_status_effect mv

(* ------------------------------------------------------------ *)
let take_turn b mv ~player_turn =
  (* 1. choose arrays & setters *)
  let att_arr, def_arr, set_att, set_def =
    if player_turn then
      ( b.player,
        b.opponent,
        (fun p -> b.player.(0) <- p),
        fun p -> b.opponent.(0) <- p )
    else
      ( b.opponent,
        b.player,
        (fun p -> b.opponent.(0) <- p),
        fun p -> b.player.(0) <- p )
  in
  let attacker = att_arr.(0) in

  (* 2. apply attacker’s current status *)
  (match Pokemon.get_status attacker with
  | 1 ->
      Printf.printf "%s is asleep and misses its turn.\n"
        (Pokemon.get_name attacker);
      set_att (Pokemon.set_status attacker 0)
  | 3 ->
      Printf.printf "%s is paralyzed and can't move this turn.\n"
        (Pokemon.get_name attacker)
  | 2 ->
      Printf.printf "%s is poisoned and takes 5 damage!\n"
        (Pokemon.get_name attacker);
      let a' = Pokemon.take_damage attacker 5 in
      set_att (Pokemon.set_status a' 2)
  | _ -> ());

  (* re‑grab possibly updated mon *)
  let attacker = att_arr.(0) in
  let defender = def_arr.(0) in

  (* 3. skip turn if still asleep/paralyzed *)
  if Pokemon.get_status attacker = 1 || Pokemon.get_status attacker = 3 then ()
  else if Random.int 100 >= Move.get_accuracy mv then
    Printf.printf "%s's %s missed!\n"
      (Pokemon.get_name attacker)
      (Move.get_name mv)
  else begin
    (* 4. compute and apply damage *)
    let base = Pokemon.get_attack attacker * Move.get_power mv / 10 in
    let atk_t = Move.get_mtype mv in
    let def_t = Pokemon.get_ptype defender in
    let dmg =
      if List.mem atk_t (PType.get_weak def_t) then base * 2
      else if List.mem atk_t (PType.get_resistant def_t) then base / 2
      else base
    in
    Printf.printf "%s used %s! It dealt %d damage!\n"
      (Pokemon.get_name attacker)
      (Move.get_name mv) dmg;
    let d' = Pokemon.take_damage defender dmg in
    set_def d';

    (* 5. inflict new status on defender, if any *)
    match status_of_move mv with
    | Some s -> set_def (Pokemon.set_status d' s)
    | None -> ()
  end;

  (* 6. after damage, auto‑switch any fainted active Pokémon *)
  handle_faint att_arr;
  handle_faint def_arr

(* ------------------------------------------------------------ *)
let heal p pct =
  let max_hp = Pokemon.get_max_hp p in
  let new_hp = min max_hp (Pokemon.get_hp p + (max_hp * pct / 100)) in
  { p with hp = new_hp }

let use_item b (p : Pokemon.t) idx =
  if idx < 0 || idx >= Array.length b.items then invalid_arg "bad item index";
  let healed = heal p b.items.(idx).heal in
  let items' =
    Array.init
      (Array.length b.items - 1)
      (fun i -> if i < idx then b.items.(i) else b.items.(i + 1))
  in
  if b.player.(0) == p then b.player.(0) <- healed;
  { b with items = items' }
