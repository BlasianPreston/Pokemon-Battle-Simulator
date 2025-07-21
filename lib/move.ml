open PType

type t = {
  name : string;
  mtype : PType.t;
  pow : int;
  acc : int;
  status_effect : int option;
}

(* Four‑argument constructor; status_effect defaults to None *)
let create_move name mtype pow acc =
  { name; mtype; pow; acc; status_effect = None }

let get_name move = move.name

(* Return raw type‐name for compatibility with tests *)
let get_mtype move = get_type move.mtype
let get_power move = move.pow
let get_accuracy move = move.acc
let get_status_effect move = move.status_effect

let get_all_moves =
  [|
    {
      name = "Scratch";
      mtype = create_type "normal";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Ember";
      mtype = create_type "fire";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Growl";
      mtype = create_type "normal";
      pow = 10;
      acc = 10;
      status_effect = None;
    };
    {
      name = "Tackle";
      mtype = create_type "normal";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Water Gun";
      mtype = create_type "water";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Tail Whip";
      mtype = create_type "normal";
      pow = 10;
      acc = 10;
      status_effect = None;
    };
    {
      name = "Vine Whip";
      mtype = create_type "grass";
      pow = 45;
      acc = 100;
      status_effect = None;
    };
    {
      name = "Wrap";
      mtype = create_type "normal";
      pow = 15;
      acc = 15;
      status_effect = None;
    };
    {
      name = "Twister";
      mtype = create_type "dragon";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Slam";
      mtype = create_type "normal";
      pow = 80;
      acc = 80;
      status_effect = None;
    };
    {
      name = "Quick Attack";
      mtype = create_type "normal";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Thunder Shock";
      mtype = create_type "electric";
      pow = 40;
      acc = 40;
      status_effect = Some 3;
    };
    {
      name = "Thunderbolt";
      mtype = create_type "electric";
      pow = 90;
      acc = 90;
      status_effect = Some 3;
    };
    {
      name = "Gust";
      mtype = create_type "flying";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Wing Attack";
      mtype = create_type "flying";
      pow = 60;
      acc = 60;
      status_effect = None;
    };
    {
      name = "Hyper Fang";
      mtype = create_type "normal";
      pow = 80;
      acc = 80;
      status_effect = None;
    };
    {
      name = "Low Kick";
      mtype = create_type "fighting";
      pow = 50;
      acc = 50;
      status_effect = None;
    };
    {
      name = "Karate Chop";
      mtype = create_type "fighting";
      pow = 50;
      acc = 50;
      status_effect = None;
    };
    {
      name = "Submission";
      mtype = create_type "fighting";
      pow = 80;
      acc = 80;
      status_effect = None;
    };
    {
      name = "Lick";
      mtype = create_type "ghost";
      pow = 30;
      acc = 30;
      status_effect = Some 1;
    };
    {
      name = "Night Shade";
      mtype = create_type "ghost";
      pow = 40;
      acc = 40;
      status_effect = None;
    };
    {
      name = "Shadow Ball";
      mtype = create_type "ghost";
      pow = 80;
      acc = 80;
      status_effect = None;
    };
    {
      name = "Mud Slap";
      mtype = create_type "rock";
      pow = 20;
      acc = 20;
      status_effect = None;
    };
    {
      name = "Dig";
      mtype = create_type "rock";
      pow = 80;
      acc = 80;
      status_effect = None;
    };
    {
      name = "Rock Throw";
      mtype = create_type "ground";
      pow = 50;
      acc = 50;
      status_effect = None;
    };
    {
      name = "Earthquake";
      mtype = create_type "ground";
      pow = 100;
      acc = 100;
      status_effect = None;
    };
  |]
