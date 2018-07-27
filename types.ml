open Utils
open Quadtree

(* A room in the dungeon *)
type room = {
  coord: vec;
  title: string; (* Short description of the room *)
  interaction: (game_state -> game_state); (* Interaction function for the room, use `no_action` for no action *)
  event: (game_state -> game_state); (* Events happen when you enter a room, changes the game state *)
}

(* Types of items denote where they can be used
 * Weapon (atk) - used in battle
 * Armor (def) - equippable items
 * Item (func) - used in or out of battle
 * Loot (value) - acquired and meant to be sold
 *)
and item_type =
  | Weapon of int
  | Armor of int
  | Item of (game_state -> game_state)
  | Loot of int

(* Item metadata container *)
and item = {
  name: string;
  count: int;
  item_type: item_type;
}

(* Current state of the player *)
and game_state = {
  room: room;
  map: room option quad_node;
  gold: int;
  inventory: item list;
  health: int;
  weapon: item option;
  armor: item option;
  flags: string list;
}

(* Fancy Room Option with the ability to have conditional decisions *)
and 'a option = Some of 'a | Undecided of (game_state -> 'a option)  | None
