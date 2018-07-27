open Str
open Utils
open Types

(* Converts item type to string for printing *)
let string_of_item_type : item_type -> string = function
  | Weapon atk -> "Weapon +" ^ string_of_int atk
  | Armor def -> "Armor +" ^ string_of_int def
  | Item _ -> "Item"
  | Loot _ -> "Loot"

(* Prints out item information *)
let print_item ({name; count; item_type; _}: item) : unit =
  if count = 0 then ()
  else println (name ^ (
    if count = 1 then "" else "x" ^ string_of_int count
  ) ^ " (" ^ string_of_item_type item_type ^ ")")

(* check if the player has an item *)
let has_item (state: game_state) (name: string) : bool =
  List.exists (fun i -> i.name = name) state.inventory

(* check if the player has a specific items *)
let num_item (state: game_state) (name: string) : int =
  (List.find (fun i -> i.name = name) state.inventory).count

(* Remove an item from the player's inventory *)
let remove_item (state: game_state) (name: string) (count: int) : game_state = {
  state with inventory =
    (* Remove count from specified item *)
    List.map (fun i -> if i.name = name then {i with count = i.count - count} else i) state.inventory |>
    (* Remove items with zero length *)
    List.filter (fun i -> i.count > 0)
}

(* Either increment an item or add it to the inventory *)
let add_item (state: game_state) (item: item) (count: int) : game_state = {
  state with inventory =
    if has_item state item.name then
      (*  increment the count of the specific item if we have it*)
      List.map (fun i -> if i.name = item.name then {i with count = i.count + count} else i) state.inventory
    else
      (* otherwise if we don't have the item in the inventory, add it *)
      state.inventory @ [{item with count}]
}

(* Lookup item in inventory by a string
 * Given a small string, finds an item with a matching set of characters *)
let item_from_str ({inventory; _}: game_state) (str: string) : item option =
  (* This is a little complicated...
   * Takes a string, breaks it into a list of characters
   * Joins these characters with ".*"
   * Uses this new string as a pattern for matching other strings
   * "foo bar" -> ".*f.*o.*o.* .*b.*a.*r.*"
   * This means the string "cprsd" produce a regex to match "copper sword"
   * *)
  let pattern = Str.regexp (".*" ^ List.fold_right (fun a b -> a ^ ".*" ^ b) (Str.split (Str.regexp "") (String.lowercase_ascii str)) "") in
  (* Helper function for checking if the pattern matches *)
  let contains_str ({name; _}: item) = Str.string_match pattern (String.lowercase_ascii name) 0 in

  (* Determine if we have a matching item *)
  if List.exists contains_str inventory then
    Some (List.find contains_str inventory)
  else
    None
