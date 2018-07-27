open Types
open Quadtree
open Utils
open Item
open Core
open Helpers
open Flags

(* List of items *)
let copper_key_item = {
  name = "Copper Key";
  count = 0;
  item_type = Item no_use_yet;
}

let candle_item = {
  name = "Candle";
  count = 0;
  item_type = Item no_use_yet;
}

let candle_lit_item = {
  name = "Lit Candle";
  count = 0;
  item_type = Item no_use_yet;
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
  if has_flag state "StartingRoomKey" then
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
      title = "Hidden Closet";
      coord = {x = -1; y = 0};
      interaction = (fun state -> (* if we have a copper key, do nothing *)
        if has_flag state "CandleA" then
          no_action state
        else (
          println "Acquired a 'Candle'";
          add_item (add_flag state "CandleA") candle_item 1
        )
      );
      event = no_event;
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
  flags = [];
};;

println "Type 'help' for more info, 'quit' to quit";;
game_loop initial_state

