(* Any miscellaneous functions that do not really belong in other files are here
 * They are simple utility functions that are specific to this project
 *)
open Types
open Utils
open Quadtree
open Item

(* Function to be used when a room has no action *)
let no_action (state: game_state) : game_state =
  println "Nothing to do here...";
  state

(* Function to be used when a room has no event *)
let no_event (state: game_state) : game_state = state

(* Helper function to be used for items with quest value *)
let no_use_yet (state: game_state) : game_state =
  println "This will be used eventually";
  state

(* Get the a room (not an option) from a coordinate, will raise an error for undecided/none *)
let get_room (node: room option quad_node) (pos: vec) =
  match quad_find node pos with
    | Some room -> room
    | _ -> raise Not_found

(* Gets the offensive power of the player *)
let get_player_attack (state: game_state) : int =
  let rec helper (weapon: item option) : int = match weapon with
    | Some {item_type = Weapon atk; _} -> atk
    | Some _ -> 0
    | None -> 0
    | Undecided fn -> helper (fn state) in
  helper state.weapon

(* Gets the defensive power of the player *)
let get_player_defense (state: game_state) : int =
  let rec helper (armor: item option) : int = match armor with
    | Some {item_type = Armor def; _} -> def
    | Some _ -> 0
    | None -> 0
    | Undecided fn -> helper (fn state) in
  helper state.armor

(* Helper functions for (d)equipping weapons and armor *)
let equip_weapon (state: game_state) (item: item) : game_state =
  {(remove_item state item.name 1) with weapon = Some item}

let dequip_weapon (state: game_state) : game_state =
  let rec helper (weapon: item option) : game_state = match weapon with
    | Some weapon ->
      {(remove_item state weapon.name 1) with weapon = None}
    | Undecided fn -> helper (fn state)
    | None -> state in
  helper state.weapon

let equip_armor (state: game_state) (item: item) : game_state =
  {(remove_item state item.name 1) with armor = Some item}

let dequip_armor (state: game_state) : game_state =
  let rec helper (armor: item option) : game_state = match armor with
    | Some armor ->
      {(remove_item state armor.name 1) with armor = None}
    | Undecided fn -> helper (fn state)
    | None -> state in
  helper state.armor


