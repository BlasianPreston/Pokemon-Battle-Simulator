open Final_project
open Random
open PType
open Pokemon
open Move
open Item
open Battle
open Tsdl
open Tsdl_ttf

let () = Random.self_init ()

(* Sprite loading *)
let sprites = Hashtbl.create 10
let battle_background = ref None
let menu_state = ref `Main
let quit = ref false
let font_path = "assets/fonts/Helvetica.ttf"
let font = ref None

let load_sprite renderer name =
  let path = "assets/sprites/" ^ String.lowercase_ascii name ^ ".bmp" in
  try
    let surface =
      match Sdl.load_bmp path with
      | Ok s -> s
      | Error (`Msg e) -> failwith e
    in
    let texture =
      match Sdl.create_texture_from_surface renderer surface with
      | Ok t -> t
      | Error (`Msg e) -> failwith e
    in
    let _ = Sdl.free_surface surface in
    Printf.printf "Successfully loaded sprite: %s\n" name;
    texture
  with e ->
    Printf.printf "Error loading sprite %s: %s\n" name (Printexc.to_string e);
    raise e

(* Button type for clickable UI elements *)
type button = {
  x : int;
  y : int;
  width : int;
  height : int;
  text : string;
  action : unit -> unit;
}

(* Check if a point is inside a button *)
let is_point_in_button x y button =
  x >= button.x
  && x <= button.x + button.width
  && y >= button.y
  && y <= button.y + button.height

(* Draw a button *)
let draw_button renderer button =
  let rect =
    Sdl.Rect.create ~x:button.x ~y:button.y ~w:button.width ~h:button.height
  in
  (* Draw button background *)
  let _ = Sdl.set_render_draw_color renderer 200 200 200 255 in
  let _ = Sdl.render_fill_rect renderer (Some rect) in
  let _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
  let _ = Sdl.render_draw_rect renderer (Some rect) in

  (* Draw text *)
  match !font with
  | Some f ->
      let text_surface =
        match
          Ttf.render_text_solid f button.text
            (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255)
        with
        | Ok s -> s
        | Error (`Msg e) ->
            Printf.printf "Error rendering text: %s\n%!" e;
            failwith e
      in
      let text_texture =
        match Sdl.create_texture_from_surface renderer text_surface with
        | Ok t -> t
        | Error (`Msg e) ->
            Printf.printf "Error creating texture: %s\n%!" e;
            failwith e
      in
      let _ = Sdl.free_surface text_surface in
      let text_rect =
        Sdl.Rect.create ~x:(button.x + 5) ~y:(button.y + 15)
          ~w:(button.width - 10) ~h:20
      in
      let _ =
        Sdl.render_copy renderer text_texture
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:(button.width - 10) ~h:20)
          ~dst:text_rect
      in
      let _ = Sdl.destroy_texture text_texture in
      ()
  | None ->
      (* Fallback to rectangle if font not loaded *)
      let text_rect =
        Sdl.Rect.create ~x:(button.x + 5) ~y:(button.y + 15)
          ~w:(button.width - 10) ~h:20
      in
      let _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
      let _ = Sdl.render_fill_rect renderer (Some text_rect) in
      ()

(* Draw a list of buttons *)
let draw_buttons renderer buttons = List.iter (draw_button renderer) buttons

(* Handle button clicks *)
let handle_click x y buttons =
  List.iter
    (fun button -> if is_point_in_button x y button then button.action ())
    buttons

(* Pokemon selection screen *)
let choose_pokemon_screen pool n =
  Printf.printf "Selecting %d random Pokemon from pool\n%!" n;
  let _selected = ref [] in
  let shuffled = Array.copy pool in
  for i = Array.length shuffled - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = shuffled.(i) in
    shuffled.(i) <- shuffled.(j);
    shuffled.(j) <- tmp
  done;
  Array.sub shuffled 0 n

(* Primary types *)
let types : PType.t array =
  [|
    create_type "fire";
    create_type "water";
    create_type "grass";
    create_type "electric";
    create_type "flying";
    create_type "normal";
    create_type "fighting";
    create_type "ghost";
    create_type "ground";
    create_type "rock";
    create_type "dragon";
  |]

(* Starter pool *)
let starters : Pokemon.t array =
  [|
    create "Charmander" 10 200 0 types.(0) charmander_moves;
    create "Squirtle" 10 200 0 types.(1) squirtle_moves;
    create "Bulbasaur" 10 200 0 types.(2) bulbasaur_moves;
    create "Pikachu" 10 200 0 types.(3) pikachu_moves;
    create "Pidgey" 10 200 0 types.(4) pidgey_moves;
    create "Rattata" 10 200 0 types.(5) rattata_moves;
    create "Machop" 10 200 0 types.(6) machop_moves;
    create "Gastly" 10 200 0 types.(7) gastly_moves;
    create "Diglett" 10 200 0 types.(8) diglett_moves;
    create "Geodude" 10 200 0 types.(9) geodude_moves;
    create "Dratini" 10 200 0 types.(10) dratini_moves;
  |]

(* Select random team *)
let select_random_team n pool =
  let shuffled = Array.copy pool in
  for i = Array.length shuffled - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = shuffled.(i) in
    shuffled.(i) <- shuffled.(j);
    shuffled.(j) <- temp
  done;
  Array.sub shuffled 0 n

(* Draw HP bars *)
let draw_hp_bar renderer x y current max =
  let width = 200 in
  let height = 20 in
  let rect = Sdl.Rect.create ~x ~y ~w:width ~h:height in
  let _ = Sdl.set_render_draw_color renderer 255 0 0 255 in
  let _ = Sdl.render_fill_rect renderer (Some rect) in
  let health_rect =
    Sdl.Rect.create ~x ~y ~w:(width * current / max) ~h:height
  in
  let _ = Sdl.set_render_draw_color renderer 0 255 0 255 in
  let _ = Sdl.render_fill_rect renderer (Some health_rect) in
  ()

(** [auto_switch team] – If [team.(0)] is at 0 HP, swap in the first healthy
    reserve. – Returns [Some msg] describing what happened, or [None]. *)
let auto_switch (team : Pokemon.t array) : string option =
  if Pokemon.get_hp team.(0) > 0 then None
  else
    let fainted = Pokemon.get_name team.(0) in
    let rec find i =
      if i >= Array.length team then None
      else if Pokemon.get_hp team.(i) > 0 then Some i
      else find (i + 1)
    in
    match find 1 with
    | None -> Some (Printf.sprintf "%s fainted! No Pokemon left!" fainted)
    | Some idx ->
        let next = Pokemon.get_name team.(idx) in
        let tmp = team.(0) in
        team.(0) <- team.(idx);
        team.(idx) <- tmp;
        Some (Printf.sprintf "%s fainted!  Go, %s!" fainted next)

(* Battle state *)
type battle_state = {
  player : Pokemon.t array;
  opponent : Pokemon.t array;
  items : Item.t array;
  player_turn : bool;
  message : string;
  message_timer : int;
}

let battle_state = ref None

(* Calculate damage for a move *)
let calculate_damage attacker defender move =
  let base_damage = Pokemon.get_attack attacker * Move.get_power move / 10 in
  let move_type = Move.get_mtype move in
  let def_type = Pokemon.get_ptype defender in
  let type_multiplier =
    if List.mem move_type (PType.get_weak def_type) then 2.0
    else if List.mem move_type (PType.get_resistant def_type) then 0.5
    else 1.0
  in
  let message =
    if type_multiplier = 2.0 then "It's super effective!"
    else if type_multiplier = 0.5 then "It's not very effective..."
    else ""
  in
  let damage = int_of_float (float_of_int base_damage *. type_multiplier) in
  (damage, message)

(* Set HP of a Pokemon *)
let set_hp pokemon new_hp =
  Pokemon.take_damage pokemon (Pokemon.get_hp pokemon - new_hp)

(* Helper for if pokemon is fainted*)
let has_healthy_reserve team =
  let ok = ref false in
  for i = 1 to Array.length team - 1 do
    if Pokemon.get_hp team.(i) > 0 then ok := true
  done;
  !ok

(* Execute a move *)
let execute_move attacker defender move =
  let damage, type_message = calculate_damage attacker defender move in
  let new_hp = max 0 (get_hp defender - damage) in
  let message =
    Printf.sprintf "%s used %s! %s It dealt %d damage!"
      (Pokemon.get_name attacker)
      (Move.get_name move) type_message damage
  in

  (* Update HP directly in the battle state *)
  (match !battle_state with
  | Some battle ->
      if defender == battle.player.(0) then
        battle.player.(0) <- set_hp battle.player.(0) new_hp
      else battle.opponent.(0) <- set_hp battle.opponent.(0) new_hp;

      (* Update the battle state *)
      battle_state := Some { battle with message; message_timer = 40 }
  | None -> ());
  message

let switch_ai_pokemon battle =
  let available_pokemon =
    Array.to_list battle.opponent |> List.filter (fun p -> get_hp p > 0)
  in
  match available_pokemon with
  | [] -> "The opponent has no Pokemon left!"
  | new_team -> (
      let new_active = List.hd new_team in
      let remaining = List.tl new_team in
      let new_opponent = Array.of_list (new_active :: remaining) in
      match !battle_state with
      | Some b ->
          battle_state := Some { b with opponent = new_opponent };
          Printf.sprintf "Opponent switched to %s!"
            (Pokemon.get_name new_active)
      | None -> "Error: No active battle state.")

(* Track if switching is needed *)
let player_needs_switch = ref false
let force_team_switch = ref false
let ai_needs_switch = ref false
let game_over = ref false
let game_over_timer = ref 0

(* Check if all Pokemon in a team are fainted *)
let is_team_defeated team = Array.for_all (fun p -> get_hp p <= 0) team

(* Check for game over and display message *)
let check_game_over battle =
  if is_team_defeated battle.player then (
    battle_state :=
      Some
        {
          battle with
          message = "You lose! The opponent defeated all your Pokemon.";
          message_timer = 30;
        };
    game_over := true;
    game_over_timer := 30)
  else if is_team_defeated battle.opponent then (
    battle_state :=
      Some
        {
          battle with
          message = "You win! You defeated all the opponent's Pokemon.";
          message_timer = 30;
        };
    game_over := true;
    game_over_timer := 30)

(* Ensure player switches if their Pokemon is fainted *)
let ensure_player_switch battle =
  if get_hp battle.player.(0) <= 0 then
    let remaining_pokemon =
      Array.to_list battle.player |> List.filter (fun p -> get_hp p > 0)
    in
    match remaining_pokemon with
    | [] -> check_game_over battle (* No healthy Pokemon left, game over *)
    | new_team ->
        (* Force player to switch *)
        player_needs_switch := true;
        force_team_switch := true;
        menu_state := `Team;
        battle_state :=
          Some
            {
              battle with
              message = "Your Pokemon fainted! Choose another.";
              message_timer = 10;
              player_turn = true;
            }

(* Ensure AI switches if their Pokemon is fainted *)
let ensure_ai_switch battle =
  if get_hp battle.opponent.(0) <= 0 then
    let remaining_pokemon =
      Array.to_list battle.opponent |> List.filter (fun p -> get_hp p > 0)
    in
    match remaining_pokemon with
    | [] -> check_game_over battle
    | new_team ->
        let new_active = List.hd new_team in
        let new_opponent = Array.of_list new_team in
        battle_state :=
          Some
            {
              battle with
              opponent = new_opponent;
              message =
                Printf.sprintf "Opponent switched to %s!"
                  (Pokemon.get_name new_active);
              player_turn = true;
              message_timer = 10;
            }

(* AI move selection - with auto-switch if fainted *)
let select_ai_move battle =
  ensure_ai_switch battle;
  if get_hp battle.opponent.(0) > 0 then
    let moves = Pokemon.get_moves battle.opponent.(0) in
    let move = moves.(Random.int (Array.length moves)) in
    execute_move battle.opponent.(0) battle.player.(0) move
  else "The opponent has no Pokemon left!"

(* Handle move selection *)
let handle_move_selection battle move =
  if !player_needs_switch then (
    battle_state :=
      Some
        {
          battle with
          message = "Your Pokemon fainted! Choose another.";
          message_timer = 10;
        };
    menu_state := `Team)
  else if not battle.player_turn then
    battle_state :=
      Some { battle with message = "It's not your turn!"; message_timer = 10 }
  else if get_hp battle.player.(0) <= 0 then ensure_player_switch battle
  else
    (* Player attack *)
    let player_message =
      execute_move battle.player.(0) battle.opponent.(0) move
    in
    battle_state :=
      Some { battle with message = player_message; player_turn = false };
    menu_state := `Main;
    check_game_over battle

(* Draw main menu - with condition for forced switch *)
let draw_main_menu renderer battle =
  let buttons = ref [] in
  if not !player_needs_switch then (
    let attack_button =
      {
        x = 50;
        y = 450;
        width = 150;
        height = 50;
        text = "Attack";
        action = (fun () -> menu_state := `Moves);
      }
    in
    let switch_button =
      {
        x = 250;
        y = 450;
        width = 150;
        height = 50;
        text = "Switch";
        action = (fun () -> menu_state := `Team);
      }
    in
    buttons := [ attack_button; switch_button ];
    draw_buttons renderer !buttons);
  !buttons

(* Draw move menu *)
let draw_move_menu renderer battle =
  let buttons = ref [] in
  let moves = Pokemon.get_moves battle.player.(0) in
  Array.iteri
    (fun i move ->
      let row = i / 2 in
      let col = i mod 2 in
      let x = 50 + (col * 250) in
      let y = 450 + (row * 60) in
      buttons :=
        {
          x;
          y;
          width = 200;
          height = 50;
          text = Move.get_name move;
          action = (fun () -> handle_move_selection battle move);
        }
        :: !buttons)
    moves;
  draw_buttons renderer !buttons;
  !buttons

(* Draw team menu - only allows switching to healthy Pokemon *)
let draw_team_menu renderer battle =
  let buttons = ref [] in
  Array.iteri
    (fun i p ->
      let row = i / 3 in
      let col = i mod 3 in
      let x = 50 + (col * 250) in
      let y = 450 + (row * 60) in
      if get_hp p > 0 then (* Only show healthy Pokemon for switching *)
        buttons :=
          {
            x;
            y;
            width = 200;
            height = 50;
            text = Pokemon.get_name p;
            action =
              (fun () ->
                if !player_needs_switch || battle.player_turn then
                  if i <> 0 then (
                    (* Switch the Pokemon *)
                    let tmp = battle.player.(0) in
                    battle.player.(0) <- battle.player.(i);
                    battle.player.(i) <- tmp;
                    player_needs_switch := false;
                    force_team_switch := false;
                    menu_state := `Main;
                    battle_state :=
                      Some
                        {
                          battle with
                          message = "You switched Pokemon.";
                          player_turn = true;
                        }));
          }
          :: !buttons)
    battle.player;
  draw_buttons renderer !buttons;
  !buttons

(* Draw battle scene *)
let draw_battle_scene renderer battle =
  (* Clear the renderer *)
  ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
  ignore (Sdl.render_clear renderer);

  (* Draw battle background *)
  (match !battle_background with
  | Some bg ->
      ignore
        (Sdl.render_copy renderer bg
           ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:800 ~h:373)
           ~dst:(Sdl.Rect.create ~x:0 ~y:0 ~w:800 ~h:373));
      ignore (Sdl.set_render_draw_color renderer 100 100 100 255);
      ignore
        (Sdl.render_fill_rect renderer
           (Some (Sdl.Rect.create ~x:0 ~y:400 ~w:800 ~h:200)))
  | None ->
      ignore (Sdl.set_render_draw_color renderer 200 200 200 255);
      ignore
        (Sdl.render_fill_rect renderer
           (Some (Sdl.Rect.create ~x:0 ~y:0 ~w:800 ~h:600)));
      ignore (Sdl.set_render_draw_color renderer 100 100 100 255);
      ignore
        (Sdl.render_fill_rect renderer
           (Some (Sdl.Rect.create ~x:0 ~y:400 ~w:800 ~h:200))));

  (* Draw Pokemon sprites *)
  (try
     let player_sprite =
       Hashtbl.find sprites (Pokemon.get_name battle.player.(0))
     in
     let enemy_sprite =
       Hashtbl.find sprites (Pokemon.get_name battle.opponent.(0))
     in
     ignore
       (Sdl.render_copy renderer player_sprite
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:128 ~h:128)
          ~dst:(Sdl.Rect.create ~x:150 ~y:200 ~w:128 ~h:128));
     ignore
       (Sdl.render_copy renderer enemy_sprite
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:128 ~h:128)
          ~dst:(Sdl.Rect.create ~x:500 ~y:100 ~w:128 ~h:128))
   with Not_found -> ());

  (* Draw Pokemon info with text *)
  (match !font with
  | Some f ->
      (* Player Pokemon info *)
      let player_name = Pokemon.get_name battle.player.(0) in
      let player_hp = get_hp battle.player.(0) in
      let player_text = Printf.sprintf "%s HP: %d/200" player_name player_hp in
      let player_surface =
        match
          Ttf.render_text_solid f player_text
            (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255)
        with
        | Ok s -> s
        | Error (`Msg e) ->
            Printf.printf "Error rendering player text: %s\n%!" e;
            failwith e
      in
      let player_texture =
        match Sdl.create_texture_from_surface renderer player_surface with
        | Ok t -> t
        | Error (`Msg e) ->
            Printf.printf "Error creating player texture: %s\n%!" e;
            failwith e
      in
      let _ = Sdl.free_surface player_surface in
      let player_rect = Sdl.Rect.create ~x:50 ~y:350 ~w:300 ~h:30 in
      let _ =
        Sdl.render_copy renderer player_texture
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:300 ~h:30)
          ~dst:player_rect
      in
      let _ = Sdl.destroy_texture player_texture in

      (* Enemy Pokemon info *)
      let enemy_name = Pokemon.get_name battle.opponent.(0) in
      let enemy_hp = get_hp battle.opponent.(0) in
      let enemy_text = Printf.sprintf "%s HP: %d/200" enemy_name enemy_hp in
      let enemy_surface =
        match
          Ttf.render_text_solid f enemy_text
            (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255)
        with
        | Ok s -> s
        | Error (`Msg e) ->
            Printf.printf "Error rendering enemy text: %s\n%!" e;
            failwith e
      in
      let enemy_texture =
        match Sdl.create_texture_from_surface renderer enemy_surface with
        | Ok t -> t
        | Error (`Msg e) ->
            Printf.printf "Error creating enemy texture: %s\n%!" e;
            failwith e
      in
      let _ = Sdl.free_surface enemy_surface in
      let enemy_rect = Sdl.Rect.create ~x:450 ~y:50 ~w:300 ~h:30 in
      let _ =
        Sdl.render_copy renderer enemy_texture
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:300 ~h:30)
          ~dst:enemy_rect
      in
      let _ = Sdl.destroy_texture enemy_texture in

      (* Draw battle message (supporting multiline) *)
      let message_lines = String.split_on_char '\n' battle.message in
      List.iteri
        (fun i line ->
          let message_surface =
            match
              Ttf.render_text_solid f line
                (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255)
            with
            | Ok s -> s
            | Error (`Msg e) ->
                Printf.printf "Error rendering message: %s\n%!" e;
                failwith e
          in
          let message_texture =
            match Sdl.create_texture_from_surface renderer message_surface with
            | Ok t -> t
            | Error (`Msg e) ->
                Printf.printf "Error creating message texture: %s\n%!" e;
                failwith e
          in
          let _ = Sdl.free_surface message_surface in
          let message_rect =
            Sdl.Rect.create ~x:50 ~y:(400 + (i * 25)) ~w:700 ~h:25
          in
          let _ = Sdl.render_copy renderer message_texture ~dst:message_rect in
          Sdl.destroy_texture message_texture)
        message_lines
  | None -> ());

  (* Draw HP bars *)
  draw_hp_bar renderer 50 380 (get_hp battle.player.(0)) 200;
  draw_hp_bar renderer 450 80 (get_hp battle.opponent.(0)) 200;

  (* Draw appropriate menu based on state *)
  let current_buttons =
    if !player_needs_switch then draw_team_menu renderer battle
    else if battle.message_timer > 0 then []
    else
      match !menu_state with
      | `Main -> draw_main_menu renderer battle
      | `Moves -> draw_move_menu renderer battle
      | `Team -> draw_team_menu renderer battle
  in

  (* Present the renderer *)
  ignore (Sdl.render_present renderer);
  current_buttons

(* Menu state *)
type menu_state =
  [ `Main
  | `Moves
  | `Team
  | `Selection
  ]

let menu_state = ref `Main

(* Team selection state *)
type selection_state = {
  available : Pokemon.t array;
  selected : Pokemon.t list;
  max_selected : int;
}

let selection_state = ref None

let draw_button_with_color renderer button bg_color =
  let rect =
    Sdl.Rect.create ~x:button.x ~y:button.y ~w:button.width ~h:button.height
  in
  let r, g, b = bg_color in
  let _ = Sdl.set_render_draw_color renderer r g b 255 in
  let _ = Sdl.render_fill_rect renderer (Some rect) in
  let _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
  let _ = Sdl.render_draw_rect renderer (Some rect) in
  match !font with
  | Some f ->
      let text_surface =
        match
          Ttf.render_text_solid f button.text
            (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255)
        with
        | Ok s -> s
        | Error (`Msg e) ->
            Printf.printf "Error rendering text: %s\n%!" e;
            failwith e
      in
      let text_texture =
        match Sdl.create_texture_from_surface renderer text_surface with
        | Ok t -> t
        | Error (`Msg e) ->
            Printf.printf "Error creating texture: %s\n%!" e;
            failwith e
      in
      let _ = Sdl.free_surface text_surface in
      let text_rect =
        Sdl.Rect.create ~x:(button.x + 5) ~y:(button.y + 15)
          ~w:(button.width - 10) ~h:20
      in
      let _ =
        Sdl.render_copy renderer text_texture
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:(button.width - 10) ~h:20)
          ~dst:text_rect
      in
      let _ = Sdl.destroy_texture text_texture in
      ()
  | None ->
      let text_rect =
        Sdl.Rect.create ~x:(button.x + 5) ~y:(button.y + 15)
          ~w:(button.width - 10) ~h:20
      in
      let _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
      let _ = Sdl.render_fill_rect renderer (Some text_rect) in
      ()

(* Draw selection screen *)
let draw_selection_screen renderer state =
  ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
  ignore (Sdl.render_clear renderer);
  (match !font with
  | Some f ->
      let title = "Choose your team (click to select/deselect)" in
      let title_surface =
        match
          Ttf.render_text_solid f title (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255)
        with
        | Ok s -> s
        | Error (`Msg e) ->
            Printf.printf "Error rendering title: %s\n%!" e;
            failwith e
      in
      let title_texture =
        match Sdl.create_texture_from_surface renderer title_surface with
        | Ok t -> t
        | Error (`Msg e) ->
            Printf.printf "Error creating title texture: %s\n%!" e;
            failwith e
      in
      let _ = Sdl.free_surface title_surface in
      let title_rect = Sdl.Rect.create ~x:50 ~y:50 ~w:700 ~h:30 in
      let _ =
        Sdl.render_copy renderer title_texture
          ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:700 ~h:30)
          ~dst:title_rect
      in
      let _ = Sdl.destroy_texture title_texture in
      ()
  | None -> ());

  let buttons = ref [] in
  Array.iteri
    (fun i p ->
      let row = i / 3 in
      let col = i mod 3 in
      let x = 50 + (col * 250) in
      let y = 100 + (row * 100) in
      let is_selected = List.mem p state.selected in
      let button =
        {
          x;
          y;
          width = 200;
          height = 80;
          text = Pokemon.get_name p;
          action =
            (fun () ->
              if is_selected then
                selection_state :=
                  Some
                    {
                      state with
                      selected = List.filter (fun x -> x != p) state.selected;
                    }
              else if List.length state.selected < state.max_selected then
                selection_state :=
                  Some { state with selected = p :: state.selected });
        }
      in
      buttons := button :: !buttons;
      let bg_color = if is_selected then (100, 220, 100) else (200, 200, 200) in
      draw_button_with_color renderer button bg_color)
    state.available;

  if List.length state.selected = state.max_selected then (
    let start_button =
      {
        x = 350;
        y = 500;
        width = 100;
        height = 50;
        text = "Start Battle";
        action =
          (fun () ->
            let player_team = Array.of_list (List.rev state.selected) in
            let ai_team = select_random_team 6 starters in
            let initial_battle =
              {
                player = player_team;
                opponent = ai_team;
                items = Array.make 1 Item.def_items.(0);
                player_turn = true;
                message = "What will you do?";
                message_timer = 0;
              }
            in
            battle_state := Some initial_battle;
            menu_state := `Main);
      }
    in
    draw_button_with_color renderer start_button (120, 120, 255);
    buttons := start_button :: !buttons);

  ignore (Sdl.render_present renderer);
  !buttons

(* Main game loop *)
let () =
  Printf.printf "Starting program...\n%!";
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> failwith ("SDL init failed: " ^ e)
  | Ok () -> (
      Printf.printf "SDL initialized successfully\n%!";

      (* Initialize TTF *)
      match Ttf.init () with
      | Error (`Msg e) -> failwith ("TTF init failed: " ^ e)
      | Ok () -> (
          Printf.printf "TTF initialized successfully\n%!";

          (* Load font *)
          match Ttf.open_font font_path 20 with
          | Error (`Msg e) -> failwith ("Font loading failed: " ^ e)
          | Ok f ->
              Printf.printf "Font loaded successfully\n%!";

              font := Some f;
              let w =
                Sdl.create_window "Pokemon Battle" ~w:800 ~h:600
                  Sdl.Window.(windowed + shown)
                |> Result.get_ok
              in
              let renderer =
                Sdl.create_renderer w ~index:(-1)
                  ~flags:Sdl.Renderer.(accelerated)
                |> Result.get_ok
              in

              (* Load sprites *)
              Array.iter
                (fun p ->
                  let name = Pokemon.get_name p in
                  try
                    let sprite = load_sprite renderer name in
                    Hashtbl.add sprites name sprite
                  with e ->
                    Printf.printf "Error loading sprite for %s: %s\n%!" name
                      (Printexc.to_string e))
                starters;

              (* Load battle background *)
              (try
                 let bg = load_sprite renderer "background" in
                 Printf.printf "Successfully loaded battle background\n%!";
                 battle_background := Some bg
               with e ->
                 Printf.printf "Error loading battle background: %s\n%!"
                   (Printexc.to_string e);
                 battle_background := None);
              Printf.printf "Background loaded\n%!";

              (* Initialize selection state *)
              selection_state :=
                Some { available = starters; selected = []; max_selected = 6 };
              menu_state := `Selection;

              (* Main game loop *)
              let event = Sdl.Event.create () in
              while not !quit do
                if !game_over then
                  if
                    (* If game is over, countdown and quit *)
                    !game_over_timer > 0
                  then game_over_timer := !game_over_timer - 1
                  else quit := true
                else
                  while Sdl.poll_event (Some event) do
                    match Sdl.Event.(get event typ) with
                    | t when t = Sdl.Event.quit -> quit := true
                    | t when t = Sdl.Event.mouse_button_down -> (
                        let x = Sdl.Event.get event Sdl.Event.mouse_button_x in
                        let y = Sdl.Event.get event Sdl.Event.mouse_button_y in
                        match !menu_state with
                        | `Selection -> (
                            match !selection_state with
                            | Some state ->
                                let buttons =
                                  draw_selection_screen renderer state
                                in
                                List.iter
                                  (fun button ->
                                    if is_point_in_button x y button then
                                      button.action ())
                                  buttons
                            | None -> ())
                        | `Team -> (
                            match !battle_state with
                            | Some battle ->
                                let buttons = draw_team_menu renderer battle in
                                List.iter
                                  (fun button ->
                                    if is_point_in_button x y button then
                                      button.action ())
                                  buttons;
                                player_needs_switch := false;
                                menu_state := `Main
                                (* Go back to main *)
                            | None -> ())
                        | _ -> (
                            match !battle_state with
                            | Some battle
                              when battle.player_turn
                                   && not !player_needs_switch ->
                                let buttons =
                                  draw_battle_scene renderer battle
                                in
                                List.iter
                                  (fun button ->
                                    if is_point_in_button x y button then
                                      button.action ())
                                  buttons
                            | _ -> ()))
                    | _ -> ()
                  done;

                (* Automatically handle turn progression *)
                (match !battle_state with
                | Some battle ->
                    if !force_team_switch then
                      menu_state := `Team (* Lock in team menu *)
                    else if battle.message_timer > 0 then
                      battle_state :=
                        Some
                          {
                            battle with
                            message_timer = battle.message_timer - 1;
                          }
                    else if !player_needs_switch then
                      ensure_player_switch battle
                    else if get_hp battle.opponent.(0) <= 0 then
                      ensure_ai_switch battle
                    else if not battle.player_turn then
                      let ai_message = select_ai_move battle in
                      battle_state :=
                        Some
                          {
                            battle with
                            message = ai_message;
                            player_turn = true;
                          }
                | None -> ());

                (* Draw the current screen *)
                (match !menu_state with
                | `Selection -> (
                    match !selection_state with
                    | Some state ->
                        ignore (draw_selection_screen renderer state)
                    | None -> ())
                | _ -> (
                    match !battle_state with
                    | Some battle -> ignore (draw_battle_scene renderer battle)
                    | None -> ()));

                (* Cap at 60 FPS *)
                Sdl.delay 16l
              done;

              (* Cleanup *)
              Printf.printf "Starting cleanup...\n%!";

              Hashtbl.iter
                (fun _ sprite -> ignore (Sdl.destroy_texture sprite))
                sprites;
              (match !font with
              | Some f -> Ttf.close_font f
              | None -> ());
              Sdl.destroy_renderer renderer;
              Sdl.destroy_window w;
              Ttf.quit ();
              Sdl.quit ();
              Printf.printf "Cleanup complete\n%!"))