open Str

(* A room in the dungeon *)
type room = {
  north: room option;
  east: room option;
  south: room option;
  west: room option;
  title: string; (* Short description of the room *)
  interaction: (game_state -> game_state); (* Interaction function for the room, use `no_action` for no action *)
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

(* Flags to denote current state of the game *)
and flag = StartingRoomKey

(* Current state of the player *)
and game_state = {
  room: room;
  gold: int;
  inventory: item list;
  health: int;
  flags: flag list;
}

(* Fancy Room Option *)
and 'a option = Some of 'a | Undecided of (game_state -> 'a option)  | None

(* Print line shortcut function *)
let println (str: string) = print_string (str ^ "\n")

(* Converts item type to string for printing *)
let string_of_item_type = function
  | Weapon atk -> "Weapon +" ^ string_of_int atk
  | Armor def -> "Armor +" ^ string_of_int def
  | Item _ -> "Item"
  | Loot _ -> "Loot"

(* Prints out item information *)
let print_item ({name; count; item_type; _}: item) =
  if count = 0 then ()
  else println (name ^ (
    if count = 1 then "" else "x" ^ string_of_int count
  ) ^ " (" ^ string_of_item_type item_type ^ ")")

(* check if the player has a specific items *)
let num_item (state: game_state) (name: string) =
  (List.find (fun i -> i.name = name) state.inventory).count

(* Remove an item from the player's inventory *)
let remove_item (state: game_state) (name: string) (count: int) = {
  state with inventory =
    (* Remove count from specified item *)
    List.map (fun i -> if i.name = name then {i with count = i.count - count} else i) state.inventory |>
    (* Remove items with zero length *)
    List.filter (fun i -> i.count > 0)
}

(* Either increment an item or add it to the inventory *)
let add_item (state: game_state) (item: item) (count: int) = {
  state with inventory =
    if List.exists (fun i -> i.name = item.name) state.inventory then
      (*  increment the count of the specific item if we have it*)
      List.map (fun i -> if i.name = item.name then {i with count = i.count + count} else i) state.inventory
    else
      (* otherwise if we don't have the item in the inventory, add it *)
      state.inventory @ [item]
}


(* Add a flag to the state *)
let add_flag (state: game_state) (flag: flag) = {
  state with flags = flag::state.flags
}

(* Determine if the state has a specific flag *)
let has_flag ({flags; _}: game_state) (flag: flag) =
  List.exists ((=)flag) flags

(* Remove a flag only upon the first encounter *)
let remove_flag (state: game_state) (flag: flag) = {
  state with flags =
    let rec find = function
      | x::xs -> if x = flag then xs else x::find xs
      | [] -> []
    in find state.flags
}

(* Given a small string, finds an item with a matching set of characters *)
let item_from_str ({inventory; _}: game_state) (str: string) =
  (* This is a little complicated...
   * Takes a string, breaks it into a list of characters
   * Joins these characters with ".*"
   * Uses this new string as a pattern for matching other strings
   * "foo bar" -> "f.*o.*o.* .*b.*a.*r.*"
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

(* Remove every instance of a flag *)
let remove_flags (state: game_state) (flag: flag) = {
  state with flags = List.filter ((!=)flag) state.flags
}

(* Function to be used when a room has no action *)
let no_action (state: game_state) =
  println "Nothing to do here...";
  state

(* Helper function to be used for items with quest value *)
let no_use_yet (state: game_state) =
  println "This will be used eventually";
  state

(* List of items *)
let copper_key_item = {
  name = "Copper Key";
  count = 1;
  item_type = Item no_use_yet;
}

(* Default room the player always starts in *)
let starting_room = {
  north = None;
  south = None;
  east = None;
  west = None;
  title = "Starting Room";
  interaction = fun state -> (* if we have a copper key, do nothing *)
    if has_flag state StartingRoomKey then
      no_action state
    else (
      println "Acquired a 'Copper Key'";
      add_item (add_flag state StartingRoomKey) copper_key_item 1
    )
}

(* Initial game state for starting with nothing *)
let new_state () = {
  room = starting_room;
  gold = 0;
  health = 100;
  inventory = [];
  flags = [];
}

(* Recursive game loop where commands are entered and handled *)
let rec game_loop (state: game_state) =
  print_string "> ";
  let input = read_line () in

  (* Helper for inspecting the room *)
  let inspect () =
    println ("You are in '" ^ state.room.title ^ "'") ; game_loop state in

  (* Helper for invoking room's action *)
  let action () =
    state.room.interaction state |> game_loop in

  (* Helper for printing the player's inventory contents *)
  let inventory () =
    println "Inventory: ";
    List.iter print_item state.inventory;
    game_loop state in

  let use () =
    print_string "Find Item: ";
    let name = read_line () in
    match item_from_str state name with
      (* Handle using regular items *)
      | Some {name; item_type = Item use_fn; _} ->
          println ("Using item '" ^ name ^ "'");
          use_fn state |> game_loop

      (* Handle using weapons *)
      | Some {name; item_type = Weapon atk; _} ->
          println ("Equipping weapon '" ^ name ^ "'");
          (* TODO: implement equipping weapons *)
          game_loop state

      (* Handle using armor *)
      | Some {name; item_type = Armor def; _} ->
          println ("Equipping armor '" ^ name ^ "'");
          (* TODO: implement equipping armor *)
          game_loop state

      (* Handle invalid search *)
      | None ->
          println "No item found";
          game_loop state

      (* Handle loot/items not programmed use cases for *)
      | _ ->
          println "No usable item found";
          game_loop state in

  (* input handler *)
  match input with
    | "help" -> println "Commands:
    action/a - Invoke the room's special action
        help - This help message
 inventory/i - Displays inventory contents
      look/l - A brief description of the room
        quit - Closes the game
       use/u - Use an item (will ask for a name)
              " ; game_loop state

    | "look" -> inspect()
    | "l" -> inspect()

    | "inventory" -> inventory()
    | "i" -> inventory()

    | "action" -> action()
    | "a" -> action()

    | "use" -> use()
    | "u" -> use()

    | "quit" -> println "Thanks for playing!" ; new_state ()

    | _ -> println "Invalid Command" ; game_loop state;;

println "Type 'help' for more info, 'quit' to quit";;
game_loop (new_state ())

