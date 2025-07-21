(* test/test_final_project.ml *)

open OUnit2
open Final_project

let types : PType.t array =
  [|
    PType.create_type "fire";
    PType.create_type "water";
    PType.create_type "grass";
  |]

let starters : Pokemon.t array =
  [|
    Pokemon.create "Charmander" 10 100 0 types.(0) Pokemon.charmander_moves;
    Pokemon.create "Squirtle" 10 100 0 types.(1) Pokemon.squirtle_moves;
    Pokemon.create "Bulbasaur" 10 100 0 types.(2) Pokemon.bulbasaur_moves;
  |]

(** Helper to create a test battle from with pokemons Charmander and Squirtle *)
let init_battle_test () =
  let battle = Battle.init_battle starters.(0) starters.(1) in
  let player_team = Battle.get_player battle in
  let opponent_team = Battle.get_opponent battle in
  assert_equal 1 (Array.length player_team);
  assert_equal 1 (Array.length opponent_team);
  assert_equal "Charmander" (Pokemon.get_name player_team.(0));
  assert_equal "Squirtle" (Pokemon.get_name opponent_team.(0));
  battle

(** Test that add_pokemon correctly adds a pokemon to the player and opponent
    teams. *)
let test_add_pokemon _ =
  let battle = init_battle_test () in
  let new_mon =
    Pokemon.create "Bulbasaur" 10 100 5 types.(2) Pokemon.bulbasaur_moves
  in
  (* Add to player's team *)
  let battle_player = Battle.add_pokemon battle ~on_player:true new_mon in
  let player_team = Battle.get_player battle_player in
  let opponent_team = Battle.get_opponent battle_player in
  assert_equal 2 (Array.length player_team);
  assert_equal "Bulbasaur" (Pokemon.get_name player_team.(1));
  assert_equal 1 (Array.length opponent_team);
  (* Add to opponent's team *)
  let battle_opp = Battle.add_pokemon battle ~on_player:false new_mon in
  let opp_team = Battle.get_opponent battle_opp in
  assert_equal 2 (Array.length opp_team)

(** Test that switch swaps two pokemon in the specified team by switching their
    position in their pokemon array. *)
let test_switch _ =
  let battle = init_battle_test () in
  let extra_mon =
    Pokemon.create "Bulbasaur" 10 100 5 types.(2) Pokemon.bulbasaur_moves
  in
  let battle = Battle.add_pokemon battle ~on_player:true extra_mon in
  Battle.switch battle ~on_player:true 1;
  let player_team = Battle.get_player battle in
  assert_equal "Bulbasaur" (Pokemon.get_name player_team.(0));
  assert_equal "Charmander" (Pokemon.get_name player_team.(1));
  let extra_opp =
    Pokemon.create "Bulbasaur" 10 100 5 types.(2) Pokemon.bulbasaur_moves
  in
  let battle = Battle.add_pokemon battle ~on_player:false extra_opp in
  Battle.switch battle ~on_player:false 1;
  let opp_team = Battle.get_opponent battle in
  assert_equal "Bulbasaur" (Pokemon.get_name opp_team.(0));
  assert_equal "Squirtle" (Pokemon.get_name opp_team.(1))

(** Test that check_faint works on a fainted pokemon (has 0 health)*)
let test_check_faint _ =
  let battle = init_battle_test () in
  assert_bool "Player Pokémon should not be fainted initially"
    (not (Battle.check_faint battle ~on_player:true 0));
  let orig_mon = (Battle.get_player battle).(0) in
  let fainted_mon = Pokemon.take_damage orig_mon 200 in
  let battle = Battle.add_pokemon battle ~on_player:true fainted_mon in
  Battle.switch battle ~on_player:true 1;
  assert_bool "Player Pokémon should be fainted after damage"
    (Battle.check_faint battle ~on_player:true 0)

(** Test that take_turn correctly applies damage*)
let test_take_turn _ =
  let battle = init_battle_test () in
  let original_hp = Pokemon.get_hp (Battle.get_player battle).(0) in
  let test_opp_mv = (Pokemon.get_moves (Battle.get_opponent battle).(0)).(1) in
  let opp_mv =
    Move.create_move
      (Move.get_name test_opp_mv)
      (PType.create_type (Move.get_mtype test_opp_mv))
      (Move.get_power test_opp_mv)
      100 (* Ensures attack hits so test doesn't randomly fail *)
  in
  Battle.take_turn battle opp_mv ~player_turn:false;
  let curr_hp = Pokemon.get_hp (Battle.get_player battle).(0) in
  assert_equal (original_hp - 80) curr_hp;
  let original_hp = Pokemon.get_hp (Battle.get_opponent battle).(0) in
  let test_player_mv = (Pokemon.get_moves (Battle.get_player battle).(0)).(1) in
  let player_mv =
    Move.create_move
      (Move.get_name test_player_mv)
      (PType.create_type (Move.get_mtype test_player_mv))
      (Move.get_power test_player_mv)
      100 (* Ensures attack hits so test doesn't randomly fail *)
  in
  Battle.take_turn battle player_mv ~player_turn:true;
  let curr_hp = Pokemon.get_hp (Battle.get_opponent battle).(0) in
  assert_equal (original_hp - 20) curr_hp

(** Test that gain_xp correctly increments a pokemon's experience. *)
let test_gain_xp _ =
  let battle = init_battle_test () in
  let p = (Battle.get_player battle).(0) in
  let xp_before = Pokemon.get_xp p in
  let p_after = Pokemon.gain_xp p 10 in
  assert_equal (xp_before + 10) (Pokemon.get_xp p_after)

(* Pokemon Test *)

(** Test all Pokemon accessors *)
let test_accessors _ =
  let mon =
    Pokemon.create "Charmander" 15 100 10 types.(1) Pokemon.charmander_moves
  in
  assert_equal "Charmander" (Pokemon.get_name mon);
  assert_equal 100 (Pokemon.get_hp mon);
  assert_equal 10 (Pokemon.get_xp mon);
  assert_equal 15 (Pokemon.get_attack mon);
  assert_equal types.(1) (Pokemon.get_ptype mon)

(** Test Pokemon.take_damage for partial health loss *)
let test_take_damage_partial _ =
  let mon =
    Pokemon.create "Charmander" 10 100 0 types.(1) Pokemon.charmander_moves
  in
  let damaged = Pokemon.take_damage mon 30 in
  assert_equal 70 (Pokemon.get_hp damaged)

(** Test Pokemon.take_damage for full health loss *)
let test_take_damage_exact _ =
  let mon =
    Pokemon.create "Charmander" 10 50 0 types.(1) Pokemon.charmander_moves
  in
  let damaged = Pokemon.take_damage mon 50 in
  assert_equal 0 (Pokemon.get_hp damaged)

(** Test Pokemon.take_damage for health loss > hp *)
let test_take_damage_overkill _ =
  let mon =
    Pokemon.create "Charmander" 10 20 0 types.(2) Pokemon.charmander_moves
  in
  let damaged = Pokemon.take_damage mon 100 in
  assert_equal 0 (Pokemon.get_hp damaged)

(** Test gaining xp *)
let test_gain_positive_xp _ =
  let mon =
    Pokemon.create "Charmander" 10 100 20 types.(0) Pokemon.charmander_moves
  in
  let mon' = Pokemon.gain_xp mon 10 in
  assert_equal 30 (Pokemon.get_xp mon')

(** Test gaining 0 xp *)
let test_gain_zero_xp _ =
  let mon =
    Pokemon.create "Charmander" 10 100 20 types.(0) Pokemon.charmander_moves
  in
  let mon' = Pokemon.gain_xp mon 0 in
  assert_equal 20 (Pokemon.get_xp mon')

(** Test losing xp *)
let test_gain_negative_xp _ =
  let mon =
    Pokemon.create "Charmander" 10 100 20 types.(0) Pokemon.charmander_moves
  in
  let mon' = Pokemon.gain_xp mon (-5) in
  assert_equal 15 (Pokemon.get_xp mon')

(** Test Pokemon.is_weak = true *)
let test_is_weak_true _ =
  let fire =
    Pokemon.create "FireMon" 10 100 0 types.(0) Pokemon.charmander_moves
  in
  let water =
    Pokemon.create "WaterMon" 10 100 0 types.(1) Pokemon.squirtle_moves
  in
  assert_equal true (Pokemon.is_weak fire water)

(** Test Pokemon.is_weak = false *)
let test_is_weak_false _ =
  let grass =
    Pokemon.create "GrassMon" 10 100 0 types.(2) Pokemon.bulbasaur_moves
  in
  let fire =
    Pokemon.create "FireMon" 10 100 0 types.(0) Pokemon.charmander_moves
  in
  assert_equal false (Pokemon.is_weak fire grass)

(** Test Pokemon.is_resistant = true *)
let test_is_resistant_true _ =
  let water =
    Pokemon.create "WaterMon" 10 100 0 types.(1) Pokemon.squirtle_moves
  in
  let grass =
    Pokemon.create "GrassMon" 10 100 0 types.(2) Pokemon.bulbasaur_moves
  in
  assert_equal true (Pokemon.is_resistant grass water)

(** Test Pokemon.is_resistant = false *)
let test_is_resistant_false _ =
  let fire =
    Pokemon.create "FireMon" 10 100 0 types.(0) Pokemon.charmander_moves
  in
  let grass =
    Pokemon.create "GrassMon" 10 100 0 types.(2) Pokemon.bulbasaur_moves
  in
  assert_equal false (Pokemon.is_resistant grass fire)

(* Battle Tests *)

(** Test Battle accessor functions for player and opponent *)
let test_get_player_and_opponent _ =
  let battle = init_battle_test () in
  let player_team = Battle.get_player battle in
  let opponent_team = Battle.get_opponent battle in
  assert_equal "Charmander" (Pokemon.get_name player_team.(0));
  assert_equal "Squirtle" (Pokemon.get_name opponent_team.(0))

(** Test Battle initialization values *)
let test_init_battle_values _ =
  let mon1 = Pokemon.create "A" 10 100 0 types.(0) Pokemon.charmander_moves in
  let mon2 = Pokemon.create "B" 10 100 0 types.(1) Pokemon.bulbasaur_moves in
  let battle = Battle.init_battle mon1 mon2 in
  assert_equal "A" (Pokemon.get_name (Battle.get_player battle).(0));
  assert_equal "B" (Pokemon.get_name (Battle.get_opponent battle).(0))

(** Test Battle poke_name and poke_hp *)
let test_poke_name_hp_both_teams _ =
  let battle = init_battle_test () in
  let name_p = Battle.poke_name (Battle.get_player battle) in
  let name_o = Battle.poke_name (Battle.get_opponent battle) in
  let hp_p = Battle.poke_hp (Battle.get_player battle) in
  let hp_o = Battle.poke_hp (Battle.get_opponent battle) in
  assert_equal "Charmander" name_p;
  assert_equal "Squirtle" name_o;
  assert_equal "100" hp_p;
  assert_equal "100" hp_o

(** Test Battle.switch for opponent *)
let test_switch_opponent_team _ =
  let battle = init_battle_test () in
  let extra =
    Pokemon.create "Gastly" 10 100 0 types.(0) Pokemon.charmander_moves
  in
  let battle = Battle.add_pokemon battle ~on_player:false extra in
  Battle.switch battle ~on_player:false 1;
  let opp_team = Battle.get_opponent battle in
  assert_equal "Gastly" (Pokemon.get_name opp_team.(0));
  assert_equal "Squirtle" (Pokemon.get_name opp_team.(1))

(** Test Battle.check_faint on opponent *)
let test_check_faint_opponent _ =
  let battle = init_battle_test () in
  let fainted = Pokemon.take_damage (Battle.get_opponent battle).(0) 200 in
  let battle = Battle.add_pokemon battle ~on_player:false fainted in
  Battle.switch battle ~on_player:false 1;
  assert_bool "Opponent Pokémon should be fainted"
    (Battle.check_faint battle ~on_player:false 0)

(** Battle raises 'index out of bounds' *)
let test_switch_invalid_index _ =
  let battle = init_battle_test () in
  assert_raises (Invalid_argument "index out of bounds") (fun () ->
      Battle.switch battle ~on_player:true 3)

(** Test Battle max team size *)
let test_add_pokemon_max_team_size _ =
  let battle = init_battle_test () in
  let mon =
    Pokemon.create "Filler" 10 100 0 types.(0) Pokemon.charmander_moves
  in
  let battle = ref battle in
  for _ = 1 to 5 do
    battle := Battle.add_pokemon !battle ~on_player:true mon
  done;
  assert_equal 6 (Array.length (Battle.get_player !battle))

(** Test take_turn functions as intended with resistances *)
let test_take_turn_resistant _ =
  let attacker =
    Pokemon.create "Grass" 10 100 0 types.(2)
      [|
        {
          Move.name = "Vine Whip";
          Move.mtype = PType.create_type "grass";
          Move.pow = 10;
          Move.acc = 100;
          Move.status_effect = None;
        };
      |]
  in
  let defender =
    Pokemon.create "Water" 10 100 0 types.(1)
      [|
        {
          Move.name = "Water Gun";
          Move.mtype = PType.create_type "water";
          Move.pow = 10;
          Move.acc = 100;
          Move.status_effect = None;
        };
      |]
  in
  let battle = Battle.init_battle defender attacker in
  let hp_before = Pokemon.get_hp (Battle.get_player battle).(0) in
  let opp_mv = (Pokemon.get_moves (Battle.get_opponent battle).(0)).(0) in
  Battle.take_turn battle opp_mv ~player_turn:false;
  let hp_after = Pokemon.get_hp (Battle.get_player battle).(0) in
  assert_equal (hp_before - 20) hp_after

(** Test take_turn functions as intended with neutral moves *)
let test_take_turn_neutral _ =
  let attacker =
    Pokemon.create "Neutral" 10 100 0 types.(0)
      [|
        {
          Move.name = "Scratch";
          Move.mtype = PType.create_type "normal";
          Move.pow = 10;
          Move.acc = 100;
          Move.status_effect = None;
        };
      |]
  in
  let defender =
    Pokemon.create "Neutral" 10 100 0 types.(0)
      [|
        {
          Move.name = "Scratch";
          Move.mtype = PType.create_type "normal";
          Move.pow = 10;
          Move.acc = 100;
          Move.status_effect = None;
        };
      |]
  in
  let battle = Battle.init_battle attacker defender in
  let hp_before = Pokemon.get_hp (Battle.get_opponent battle).(0) in
  let player_mv = (Pokemon.get_moves (Battle.get_player battle).(0)).(0) in
  Battle.take_turn battle player_mv ~player_turn:true;
  let hp_after = Pokemon.get_hp (Battle.get_opponent battle).(0) in
  assert_equal (hp_before - 10) hp_after

(* PType Tests *)

(** Tests weakness and resistance of fire types *)
let test_fire_type_resistance _ =
  let fire = PType.create_type "fire" in
  assert_equal [ "grass"; "bug"; "steel"; "fire" ] (PType.get_resistant fire);
  assert_equal [ "water"; "ground"; "rock" ] (PType.get_weak fire)

(** Tests weakness and resistance of water types *)
let test_water_type_resistance _ =
  let water = PType.create_type "water" in
  assert_equal [ "fire"; "water"; "ice"; "steel" ] (PType.get_resistant water);
  assert_equal [ "electric"; "grass"; "dragon" ] (PType.get_weak water)

(** Tests weakness and resistance of normal types, with no resistant types *)
let test_normal_type_resistance _ =
  let normal = PType.create_type "normal" in
  assert_equal [] (PType.get_resistant normal);
  assert_equal [ "fighting" ] (PType.get_weak normal)

(* Grass Type Tests *)
let test_grass_type_resistance _ =
  let grass = PType.create_type "grass" in
  assert_equal
    [ "water"; "ground"; "grass"; "electric" ]
    (PType.get_resistant grass);
  assert_equal
    [ "fire"; "flying"; "bug"; "ice"; "poison" ]
    (PType.get_weak grass)

(* Electric Type Tests *)
let test_electric_type_resistance _ =
  let electric = PType.create_type "electric" in
  assert_equal [ "electric"; "flying"; "steel" ] (PType.get_resistant electric);
  assert_equal [ "ground" ] (PType.get_weak electric)

(* Flying Type Tests *)
let test_flying_type_resistance _ =
  let flying = PType.create_type "flying" in
  assert_equal [ "grass"; "fighting"; "bug" ] (PType.get_resistant flying);
  assert_equal [ "electric"; "rock"; "ice" ] (PType.get_weak flying)

(* Fighting Type Tests *)
let test_fighting_type_resistance _ =
  let fighting = PType.create_type "fighting" in
  assert_equal [ "rock"; "bug"; "dark" ] (PType.get_resistant fighting);
  assert_equal [ "flying"; "psychic"; "fairy" ] (PType.get_weak fighting)

(* Ghost Type Tests *)
let test_ghost_type_resistance _ =
  let ghost = PType.create_type "ghost" in
  assert_equal [ "poison"; "bug" ] (PType.get_resistant ghost);
  assert_equal [ "ghost"; "dark" ] (PType.get_weak ghost)

(* Ground Type Tests *)
let test_ground_type_resistance _ =
  let ground = PType.create_type "ground" in
  assert_equal [ "poison"; "rock"; "electric" ] (PType.get_resistant ground);
  assert_equal [ "water"; "grass"; "ice" ] (PType.get_weak ground)

(* Rock Type Tests *)
let test_rock_type_resistance _ =
  let rock = PType.create_type "rock" in
  assert_equal
    [ "normal"; "fire"; "poison"; "flying" ]
    (PType.get_resistant rock);
  assert_equal
    [ "water"; "grass"; "fighting"; "ground"; "steel" ]
    (PType.get_weak rock)

(* Dragon Type Tests *)
let test_dragon_type_resistance _ =
  let dragon = PType.create_type "dragon" in
  assert_equal
    [ "fire"; "water"; "grass"; "electric" ]
    (PType.get_resistant dragon);
  assert_equal [ "ice"; "dragon"; "fairy" ] (PType.get_weak dragon)

(* Move tests *)

let test_create_move _ =
  let test = Move.create_move "Scratch" (PType.create_type "normal") 40 40 in
  assert_equal test
    {
      name = "Scratch";
      mtype = PType.create_type "normal";
      pow = 40;
      acc = 40;
      status_effect = None;
    }

(** Thunder Shock should paralyze the defender (status = 3) *)
let test_status_inflict _ =
  let atk = starters.(0) (* Charmander – any mon is fine *)
  and def =
    starters.(1)
    (* Squirtle *)
  in
  let battle = Battle.init_battle atk def in

  (* Build a 100‑accuracy Thunder Shock that still carries status_effect = 3 *)
  let thundershock : Move.t =
    {
      Move.name = "Thunder Shock";
      Move.mtype = PType.create_type "electric";
      Move.pow = 40;
      Move.acc = 100;
      (* guarantee a hit *)
      Move.status_effect = Some 3;
      (* paralysis *)
    }
  in

  Battle.take_turn battle thundershock ~player_turn:true;
  let status_after = Pokemon.get_status (Battle.get_opponent battle).(0) in
  assert_equal 3 status_after

let test_get_movename _ =
  let mv = Move.create_move "Scratch" (PType.create_type "normal") 40 40 in
  assert_equal "Scratch" (Move.get_name mv)

let test_get_mtype _ =
  let mv = Move.create_move "Scratch" (PType.create_type "normal") 40 40 in
  assert_equal "normal" (Move.get_mtype mv)

let test_get_power _ =
  let mv = Move.create_move "Scratch" (PType.create_type "normal") 40 40 in
  assert_equal 40 (Move.get_power mv)

let test_get_accuracy _ =
  let mv = Move.create_move "Scratch" (PType.create_type "normal") 40 40 in
  assert_equal 40 (Move.get_accuracy mv)

(* Item Tests *)

(** Using a Potion should heal 20 HP *)
let test_item_heal _ =
  let battle = init_battle_test () in
  let p0 = (Battle.get_player battle).(0) in
  let damaged = Pokemon.take_damage p0 40 in
  (* substitute damaged mon into first slot *)
  (Battle.get_player battle).(0) <- damaged;
  let hp_before = Pokemon.get_hp damaged in
  let healed_battle = Battle.use_item battle damaged 0 in
  (* 0 = Potion *)
  let hp_after = Pokemon.get_hp (Battle.get_player healed_battle).(0) in
  assert_equal (hp_before + 20) hp_after

(** Test that when the active Pokémon faints, handle_faint swaps in the next
    healthy reserve. *)
let test_handle_faint_with_reserve _ =
  (* Start with Charmander vs. Squirtle, then add Bulbasaur as a reserve *)
  let battle = init_battle_test () in
  let battle = Battle.add_pokemon battle ~on_player:true starters.(2) in
  let player_team = Battle.get_player battle in
  (* Faint the lead Pokémon *)
  player_team.(0) <- Pokemon.take_damage player_team.(0) 200;
  (* Trigger handle_faint via opponent’s turn *)
  let opp_move = (Pokemon.get_moves (Battle.get_opponent battle).(0)).(0) in
  let mv =
    Move.create_move (Move.get_name opp_move)
      (PType.create_type (Move.get_mtype opp_move))
      (Move.get_power opp_move) 100
  in
  Battle.take_turn battle mv ~player_turn:false;
  (* Now Bulbasaur (starters.(2)) should be in front *)
  assert_equal
    (Pokemon.get_name starters.(2))
    (Pokemon.get_name (Battle.get_player battle).(0))

(** Test that when all Pokémon faint, handle_faint does nothing. *)
let test_handle_faint_no_reserve _ =
  (* Start with Charmander vs. Squirtle, then add a fainted Bulbasaur *)
  let battle = init_battle_test () in
  let fainted_reserve =
    Pokemon.create "Bulbasaur" 10 0 0 types.(2) Pokemon.bulbasaur_moves
  in
  let battle = Battle.add_pokemon battle ~on_player:true fainted_reserve in
  let player_team = Battle.get_player battle in
  (* Faint both lead and reserve *)
  player_team.(0) <- Pokemon.take_damage player_team.(0) 200;
  player_team.(1) <- Pokemon.take_damage player_team.(1) 200;
  (* Trigger handle_faint via opponent’s turn *)
  let opp_move = (Pokemon.get_moves (Battle.get_opponent battle).(0)).(0) in
  let mv =
    Move.create_move (Move.get_name opp_move)
      (PType.create_type (Move.get_mtype opp_move))
      (Move.get_power opp_move) 100
  in
  Battle.take_turn battle mv ~player_turn:false;
  (* No healthy reserves, so the lead slot remains fainted *)
  assert_equal
    (0)
    (Pokemon.get_hp (Battle.get_player battle).(0))
(**tests for sleep status*)
let test_take_turn_sleep_skips _ =
  let battle = init_battle_test () in
  let player = Battle.get_player battle in
  (* Put active Pokémon to sleep (status = 1) *)
  player.(0) <- Pokemon.set_status player.(0) 1;
  (* Use any move; it should be skipped and status reset to 0 *)
  let mv =
    let opp = (Battle.get_opponent battle).(0) in
    (Pokemon.get_moves opp).(0)
  in
  Battle.take_turn battle mv ~player_turn:true;
  assert_equal 0 (Pokemon.get_status player.(0))
(**tests for paralyze status*)
let test_take_turn_paralyze_skips _ =
  let battle = init_battle_test () in
  let player = Battle.get_player battle in
  (* Paralyze the active Pokémon (status = 3) *)
  player.(0) <- Pokemon.set_status player.(0) 3;
  (* Use any move; it should be skipped and status remain 3 *)
  let mv =
    let opp = (Battle.get_opponent battle).(0) in
    (Pokemon.get_moves opp).(0)
  in
  Battle.take_turn battle mv ~player_turn:true;
  assert_equal 3 (Pokemon.get_status player.(0))
(**tests for poison status*)
let test_take_turn_poison_takes_damage _ =
  let battle = init_battle_test () in
  let player = Battle.get_player battle in
  (* Poison the active Pokémon (status = 2) *)
  player.(0) <- Pokemon.set_status player.(0) 2;
  let hp_before = Pokemon.get_hp player.(0) in
  (* Use any move; it should apply 5 poison damage and keep status = 2 *)
  let mv =
    let opp = (Battle.get_opponent battle).(0) in
    (Pokemon.get_moves opp).(0)
  in
  Battle.take_turn battle mv ~player_turn:true;
  assert_equal (hp_before - 5) (Pokemon.get_hp player.(0));
  assert_equal 2 (Pokemon.get_status player.(0))

(**tests for is_disatvantaged*)
let test_is_disadvantaged_true _ =
  (* Charmander uses a fire-type move on Bulbasaur (grass) → should be disadvantaged *)
  let charmander = starters.(0) in
  let bulbasaur = starters.(2) in
  let move = (Pokemon.get_moves charmander).(0) in
  assert_bool "Bulbasaur should be disadvantaged by Charmander's move"
    (Pokemon.is_disadvantaged ~move ~defender:bulbasaur)

(**tests for is_disatvantaged*)
let test_is_disadvantaged_false _ =
  (* Charmander uses a fire-type move on Squirtle (water) → should NOT be disadvantaged *)
  let charmander = starters.(0) in
  let squirtle = starters.(1) in
  let move = (Pokemon.get_moves charmander).(0) in
  assert_bool "Squirtle should NOT be disadvantaged by Charmander's move"
    (not (Pokemon.is_disadvantaged ~move ~defender:squirtle))
(**test for accuracy*)
let test_accuracy_full _ =
  (* Power = 10 → accuracy = 100 *)
  assert_equal 100 (Pokemon.calculate_accuracy 10)
(**test for accuracy*)
let test_accuracy_decrease _ =
  (* Power = 20 → accuracy = 100 - ((20 - 10)/5)*5 = 90 *)
  assert_equal 90 (Pokemon.calculate_accuracy 20)
(**test for accuracy*)
let test_accuracy_floor_zero _ =
  (* Power = 200 → accuracy floors at 0 *)
  assert_equal 0 (Pokemon.calculate_accuracy 200)
(**test for accuracy*)
let test_accuracy_no_negative_power _ =
  (* Power < 10 (e.g., 5) → still 100 accuracy, as no decrement *)
  assert_equal 100 (Pokemon.calculate_accuracy 5)

let suite =
  "BattleAndSpeciesTests"
  >::: [
         "test_add_pokemon" >:: test_add_pokemon;
         "test_switch" >:: test_switch;
         "test_check_faint" >:: test_check_faint;
         "test_take_turn" >:: test_take_turn;
         "test_gain_xp" >:: test_gain_xp;
         "test_accessors" >:: test_accessors;
         "test_gain_xp_positive" >:: test_gain_positive_xp;
         "test_gain_xp_negative" >:: test_gain_negative_xp;
         "test_gain_xp_zero" >:: test_gain_zero_xp;
         "test_take_damage_partial" >:: test_take_damage_partial;
         "test_take_damage_exact" >:: test_take_damage_exact;
         "test_take_damage_overkill" >:: test_take_damage_overkill;
         "test_is_weak_true" >:: test_is_weak_true;
         "test_is_weak_false" >:: test_is_weak_false;
         "test_is_resistant_true" >:: test_is_resistant_true;
         "test_is_resistant_false" >:: test_is_resistant_false;
         "test_get_player_and_opponent" >:: test_get_player_and_opponent;
         "test_init_battle_structure" >:: test_init_battle_values;
         "test_poke_name_hp_both_teams" >:: test_poke_name_hp_both_teams;
         "test_switch_opponent_team" >:: test_switch_opponent_team;
         "test_check_faint_opponent" >:: test_check_faint_opponent;
         "test_switch_invalid_index" >:: test_switch_invalid_index;
         "test_add_pokemon_max_team_size" >:: test_add_pokemon_max_team_size;
         "test_take_turn_resistant" >:: test_take_turn_resistant;
         "test_take_turn_neutral" >:: test_take_turn_neutral;
         "test_fire_type_resistance" >:: test_fire_type_resistance;
         "test_water_type_resistance" >:: test_water_type_resistance;
         "test_grass_type_resistance" >:: test_grass_type_resistance;
         "test_electric_type_resistance" >:: test_electric_type_resistance;
         "test_flying_type_resistance" >:: test_flying_type_resistance;
         "test_normal_type_resistance" >:: test_normal_type_resistance;
         "test_fighting_type_resistance" >:: test_fighting_type_resistance;
         "test_ghost_type_resistance" >:: test_ghost_type_resistance;
         "test_ground_type_resistance" >:: test_ground_type_resistance;
         "test_rock_type_resistance" >:: test_rock_type_resistance;
         "test_dragon_type_resistance" >:: test_dragon_type_resistance;
         "test_create_move" >:: test_create_move;
         "test_get_movename" >:: test_get_movename;
         "test_get_mtype" >:: test_get_mtype;
         "test_get_power" >:: test_get_power;
         "test_get_accuracy" >:: test_get_accuracy;
         "test_status_inflict" >:: test_status_inflict;
         "test_item_heal" >:: test_item_heal;
         "test_handle_faint" >:: test_handle_faint_with_reserve;
         "test_handle_faint" >:: test_handle_faint_no_reserve;
         "sleep skips and wakes" >:: test_take_turn_sleep_skips;
         "paralyze skips turn" >:: test_take_turn_paralyze_skips;
         "poison ticks damage" >:: test_take_turn_poison_takes_damage;
         "Bulbasaur is disadvantaged by fire" >:: test_is_disadvantaged_true;
        "Squirtle is NOT disadvantaged by fire" >:: test_is_disadvantaged_false;
        "Power 10 accuracy 100" >:: test_accuracy_full;
        "Power 20  accuracy 90" >:: test_accuracy_decrease;
        "Power 200  accuracy 0" >:: test_accuracy_floor_zero;
        "Power 5  accuracy 100" >:: test_accuracy_no_negative_power;
       ]

let () = run_test_tt_main suite
