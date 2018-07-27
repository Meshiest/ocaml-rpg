open Types
open Utils
open Quadtree

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

let get_room (node: room option quad_node) (pos: vec) = 
  match quad_find node pos with
    | Some room -> room
    | _ -> raise Not_found