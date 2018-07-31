(* This is where all the actual "game" part of the game takes place
 * If you have no interest in modifying the engine and just want to make an epic tale, look no further!
 * I would encourage you to look into the battle.ml file to get a better grasp on interactivity
 * In this file, rooms, items, monsters, and game objects are developed.
 *)

(* The types module contains all the records for game state storage *)
open Types

(* The quadtree is an efficient means of storing coordinates with fast lookup and insertion times *)
open Quadtree

(* Simple utilities that are not specific to this project *)
open Utils

(* Helper functions for keeping track of items in a player's inventory *)
open Item

(* The core input handling and human interfacing part of the game *)
open Core

(* Simple utilities that are specific to this project *)
open Helpers

(* Helper functions for managing flags in the game state *)
open Flags

(* An example of an interactive event that can be created when a room is entered *)
open Battle

(* Basic rat monster *)
let rat_monster = {
  name = "Rat";
  pattern = Static [Defend; Attack; Attack; Defend];
  hitpoints = 5;
  atk = 1;
  def = 1;
  can_flee = true;
}

(* List of items *)
let silver_key_item = {
  name = "Silver Key";
  count = 0;
  item_type = Item (fun state -> println "This item unlocks silver locks"; state);
}

let copper_sword_item = {
  name = "Copper Sword";
  count = 0;
  item_type = Weapon 1;
}

let copper_key_item = {
  name = "Copper Key";
  count = 0;
  item_type = Item (fun state ->
    if state.room.title = "Dark Closet" then (
      println ("You unlock the copper lock, but your key gets stuck...\n" ^
        "Fortunately, you find some items!\n +1 Silver Key\n +1 Copper Sword\n" ^
        "Make sure you equip your sword before it's too late!"
      );
      add_item (add_item (remove_item state "Copper Key" 1) silver_key_item 1) copper_sword_item 1
    ) else (
      println "This item unlocks copper locks";
      state
    )
  );
}

let candle_item = {
  name = "Candle";
  count = 0;
  item_type = Item (fun state -> println "This item can be lit on a torch"; state);
}

let candle_lit_item = {
  name = "Lit Candle";
  count = 0;
  item_type = Item (fun state ->
    println "You put out the candle flame...";
    add_item (remove_item state "Lit Candle" 1) candle_item 1);
}

(* Root node for room storage *)
let root_node : room option quad_node = empty_node |> (* Pipe the initial node into the next room function, this can be chained *)

quad_add ({x = 0; y = 0}, Some {
  title = "Starting Room";
  coord = {x = 0; y = 0};
  event = no_event;
  interaction = fun state -> (* if we have a copper key, do nothing *)
    if has_flag state "StartingRoomKey" then
      no_action state
    else (
      println "Acquired a 'Copper Key'";
      add_item (add_flag state "StartingRoomKey") copper_key_item 1
    )
}) |>

quad_add ({x = 0; y = -1}, Undecided (fun state ->
  if has_item state "Copper Key" then
    Some {
      title = "Poorly Lit Corridor";
      coord = {x = 0; y = -1};
      interaction = (fun state ->
        if has_item state "Candle" then (
          println "Lit a Candle on a torch";
          add_item (remove_item state "Candle" 1) candle_lit_item 1
        ) else
          no_action state
      );
      event = (fun state ->
        if has_flag state "WallTorches1" then
          state
        else (
          println "The torches on the wall light up";
          add_flag state "WallTorches1"
        )
      );

    }
  else None
)) |>

quad_add ({x = 1; y = 0}, Undecided (fun state ->
  if has_item state "Silver Key" then
    Some {
      title = "Rat Nest";
      coord = {x = 1; y = 0};
      interaction = no_action;
      event = (fun state ->
        if has_flag state "KillRat" then (
          println "There's a rat corpse on the floor";
          state
        ) else (
          println "You hear a loud hiss, but it's too late...";
          enter_battle state rat_monster (fun state ->
            println "You defeated the rat!\n +10 Gold\n +1 Rat Fang";
            add_item (add_flag {state with gold = state.gold + 10} "KillRat") ({
              name = "Rat Fang";
              item_type = Loot 10;
              count = 0;
            }) 1
          ) (fun state ->
            println "Better luck next time";
            state;
          )
        )
      );

    }
  else None
)) |>

quad_add ({x = 0; y = -2}, Some {
  title = "Library of Runic Scrolls";
  coord = {x = 0; y = -2};
  interaction = (fun state ->
    println "You toggle the lever...";
    if has_flag state "LibraryLever" then
      remove_flag state "LibraryLever"
    else
      add_flag state "LibraryLever");
  event = fun state ->
    println "There is a wooden lever in the middle of the room.";
    state
}) |>

quad_add ({x = -1; y = 0}, Undecided (fun state ->
  if has_flag state "LibraryLever" then
    Some {
      title = "Dark Closet";
      coord = {x = -1; y = 0};
      interaction = (fun state -> (* if we have a copper key, do nothing *)
        if has_flag state "CandleA" then
          no_action state
        else (
          println "You pick up a 'Candle' from the floor";
          add_item (add_flag state "CandleA") candle_item 1
        )
      );
      event = (fun state ->
        if has_item state "Lit Candle" then (
          println "The light from your candle unveils a small copper lock";
          state
        ) else
          state
      );
    }
  else None
))

(* TODO: add more rooms *)

(* Initial game state for starting with nothing *)
let initial_state : game_state = {
  room = get_room root_node {x = 0; y = 0};
  map = root_node;
  gold = 0;
  health = 100;
  inventory = [];
  weapon = None;
  armor = None;
  flags = [];
};;

(* Start the game! *)
println "Type 'help' for more info, 'quit' to quit";;
game_loop initial_state

