
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

(* Current state of the player *)
and game_state = {
  room: room;
  gold: int;
  inventory: item list;
  health: int;
}

(* Fancy Room Option *)
and 'a option = Some of 'a | Undecided of (game_state -> 'a)  | None

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
let has_item (state: game_state) (name: string) =
  let rec search = function
    | x::xs -> if x.name = name then true else search xs
    | [] -> false
  in search state.inventory

(* Remove an item from the player's inventory *)
let remove_item (state: game_state) (name: string) =
  let rec filter = function
    | x::xs -> if x.name = name then filter xs else x::filter xs
    | [] -> []
  in filter state.inventory

let add_item (state: game_state) (item: item) =
  {state with inventory = state.inventory @ [item]}

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
  interaction = fun state ->
    if has_item state "Copper Key" then
      no_action state
    else (
      println "Acquired a 'Copper Key'";
      add_item state copper_key_item
    )
}

(* Initial game state for starting with nothing *)
let new_state () = {
  room = starting_room;
  gold = 0;
  health = 100;
  inventory = [];
}

(* Recursive game loop where commands are entered and handled *)
let rec game_loop (state: game_state) =
  let input = read_line() in

  (* Helper for inspecting the room *)
  let inspect () =
    println ("You are in '" ^ state.room.title ^ "'") ; game_loop state in

  (* Helper for invoking room's action *)
  let action () =
    state.room.interaction state |> game_loop in

  (* Helper for printing the player's inventory contents *)
  let inventory() =
    println "Inventory: ";
    let rec helper = function
      | x::xs -> print_item x ; helper xs
      | [] -> game_loop state
    in helper state.inventory in

  (* input handler *)
  match input with
    | "help" -> println "Commands:
    action/a - Invoke the room's special action
 inventory/i - Displays inventory contents
      look/l - A brief description of the room
        help - This help message
        quit - Closes the game" ; game_loop state

    | "look" -> inspect()
    | "l" -> inspect()

    | "inventory" -> inventory()
    | "i" -> inventory()

    | "action" -> action()
    | "a" -> action()

    | "quit" -> println "Thanks for playing!" ; new_state ()

    | _ -> println "Invalid Command" ; game_loop state;;

println "Type 'help' for more info, 'quit' to quit";;
game_loop (new_state ())

