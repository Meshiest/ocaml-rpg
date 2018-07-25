open Str

(* Print line shortcut function *)
let println (str: string) = print_string (str ^ "\n")

(* A room in the dungeon *)
type room = {
  coord: vec;
  title: string; (* Short description of the room *)
  interaction: (game_state -> game_state); (* Interaction function for the room, use `no_action` for no action *)
  event: (game_state -> game_state); (* Events happen when you enter a room, changes the game state *)
}

(* 2d vector type to store transformation information *)
and vec = {
  x: int;
  y: int;
}

(* A binary tree but... in four directions *)
and quad_node = {
  pos: vec;
  value: room option;
  north_east: quad_node option;
  north_west: quad_node option;
  south_east: quad_node option;
  south_west: quad_node option;
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
and flag = StartingRoomKey | WallTorches1

(* Current state of the player *)
and game_state = {
  room: room;
  gold: int;
  inventory: item list;
  health: int;
  flags: flag list;
}

(* Fancy Room Option with the ability to have conditional decisions *)
and 'a option = Some of 'a | Undecided of (game_state -> 'a option)  | None

(* Room quadtree lookup function *)
let rec lookup_room_node (node: quad_node) (loc: vec) : room option =
  match node with
    (* Found the room with the same pos *)
    | {pos; value; _} when pos = loc -> value

    (* Find the room in another node *)
    | {pos; north_east = Some next; _} when loc.x < pos.x && loc.y < pos.y ->
        lookup_room_node next loc
    | {pos; north_west = Some next; _} when loc.x >= pos.x && loc.y < pos.y ->
        lookup_room_node next loc
    | {pos; south_east = Some next; _} when loc.x < pos.x && loc.y >= pos.y ->
        lookup_room_node next loc
    | {pos; south_west = Some next; _} when loc.x >= pos.x && loc.y >= pos.y ->
        lookup_room_node next loc

    | _ -> None

(* Room quadtree add function *)
let rec add_room ((loc, room): vec * room option) (node: quad_node) : quad_node =
  (* Empty node *)
  let new_node = {
    value = room;
    pos = loc;
    north_east = None;
    north_west = None;
    south_east = None;
    south_west = None;
  } in

  match node with
    (* in the odd chance this is being used wrong, we'll handle it correctly anyway! *)
    | {pos; value = None; _} when pos = loc -> {node with value = room}

    (* Handle the different quadrant, create a new node if there is some room already here *)
    | {pos; north_east = Some next; _} when loc.x < pos.x && loc.y < pos.y ->
        {node with north_east = Some (add_room (loc, room) next)}
    | {pos; north_east = None; _} when loc.x < pos.x && loc.y < pos.y ->
        {node with north_east = Some new_node}

    | {pos; north_west = Some next; _} when loc.x >= pos.x && loc.y < pos.y ->
        {node with north_west = Some (add_room (loc, room) next)}
    | {pos; north_west = None; _} when loc.x >= pos.x && loc.y < pos.y ->
        {node with north_west = Some new_node}

    | {pos; south_east = Some next; _} when loc.x < pos.x && loc.y >= pos.y ->
        {node with south_east = Some (add_room (loc, room) next)}
    | {pos; south_east = None; _} when loc.x < pos.x && loc.y >= pos.y ->
        {node with south_east = Some new_node}

    | {pos; south_west = Some next; _} when loc.x >= pos.x && loc.y >= pos.y ->
        {node with south_west = Some (add_room (loc, room) next)}
    | {pos; south_west = None; _} when loc.x >= pos.x && loc.y >= pos.y ->
        {node with south_west = Some new_node}

    | _ -> println "Error with add" ; node


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
    if List.exists (fun i -> i.name = item.name) state.inventory then
      (*  increment the count of the specific item if we have it*)
      List.map (fun i -> if i.name = item.name then {i with count = i.count + count} else i) state.inventory
    else
      (* otherwise if we don't have the item in the inventory, add it *)
      state.inventory @ [item]
}


(* Add a flag to the state *)
let add_flag (state: game_state) (flag: flag) : game_state = {
  state with flags = flag::state.flags
}

(* Determine if the state has a specific flag *)
let has_flag ({flags; _}: game_state) (flag: flag) : bool =
  List.exists ((=)flag) flags

(* Remove a flag only upon the first encounter *)
let remove_flag (state: game_state) (flag: flag) : game_state = {
  state with flags =
    let rec find = function
      | x::xs -> if x = flag then xs else x::find xs
      | [] -> []
    in find state.flags
}

(* Given a small string, finds an item with a matching set of characters *)
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

(* Remove every instance of a flag *)
let remove_flags (state: game_state) (flag: flag) : game_state = {
  state with flags = List.filter ((!=)flag) state.flags
}

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

(* List of items *)
let copper_key_item = {
  name = "Copper Key";
  count = 1;
  item_type = Item no_use_yet;
}

(* Default room the player always starts in *)
let starting_room : room = {
  title = "Starting Room";
  coord = {x = 0; y = 0};
  event = no_event;
  interaction = fun state -> (* if we have a copper key, do nothing *)
    if has_flag state StartingRoomKey then
      no_action state
    else (
      println "Acquired a 'Copper Key'";
      add_item (add_flag state StartingRoomKey) copper_key_item 1
    )
}

(* Root node for room storage *)
let root_node : quad_node = {
  value = Some starting_room;
  pos = starting_room.coord;
  north_east = None;
  north_west = None;
  south_east = None;
  south_west = None;
} |> (* Pipe the initial node into the next room function, this can be chained *)
add_room ({x = 0; y = -1}, Undecided (fun state ->
  if has_flag state StartingRoomKey then
    Some {
      title = "Poorly lit corridor";
      coord = {x = 0; y = -1};
      interaction = no_action;
      event = fun state ->
        if has_flag state WallTorches1 then
          state
        else (
          println "The torches on the wall light up";
          add_flag state WallTorches1
        )

    }
  else None
))
(* TODO: add more rooms *)

let lookup_room : vec -> room option = lookup_room_node root_node

(* Initial game state for starting with nothing *)
let new_state () : game_state = {
  room = starting_room;
  gold = 0;
  health = 100;
  inventory = [];
  flags = [];
}

(* Recursive game loop where commands are entered and handled *)
let rec game_loop (state: game_state) : unit =
  print_string "> ";
  let input = read_line () in

  (* Helper for shifting a coord *)
  let offset ((x, y): int * int) : vec = {
    x = x + state.room.coord.x;
    y = y + state.room.coord.y;
  } in
  (* Helper for inspecting the room *)
  let inspect () : unit =
    (* Helper for determining if a room is open given the current state *)
    let rec is_open (room: room option) : bool = match room with
      | Undecided fn -> is_open (fn state)
      | Some room -> true
      | None -> false in

    (* Fold to create a "NSEW" string based on available rooms *)
    let directions = List.fold_left (fun str (x, y, ch) ->
      str ^ if offset (x, y) |> lookup_room |> is_open then ch else ""
    ) "" [(0, -1, "N"); (0, 1, "S"); (1, 0, "E"); (-1, 0, "W")] in

    (* Print out room title and available directions *)
    println ("You are in '" ^ state.room.title ^ "' " ^
      "(" ^ string_of_int state.room.coord.x ^ ", " ^ string_of_int state.room.coord.y ^ ") " ^
      "(" ^ directions ^ ")");
    game_loop state in

  (* Helper for invoking room's action *)
  let action () : unit =
    state.room.interaction state |> game_loop in

  (* Helper for printing the player's inventory contents *)
  let inventory () : unit =
    println "Inventory: ";
    List.iter print_item state.inventory;
    game_loop state in

  let use () : unit =
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

  let move ((x, y): int * int) : unit =
    let rec handleroom = function
      | Undecided fn -> handleroom (fn state)
      | Some room ->
          println ("You have entered '" ^ room.title ^ "'");
          (* Run the room's event, then continue the game *)
          room.event {state with room} |> game_loop
      | None ->
          println ("There's no entrance this way");
          game_loop state
    in offset (x, y) |> lookup_room |> handleroom in

  (* input handler *)
  match input with
    | "help" -> println "Commands:
  System:
        help - This help message
        quit - Closes the game

  Interaction:
    action/a - Invoke the room's special action
 inventory/i - Displays inventory contents
      look/l - A brief description of the room
       use/u - Use an item (will ask for a name)

  Movement:
     north/n - Move north if possible
     south/s - Move south if possible
      east/e - Move east if possible
      west/w - Move west if possible
                " ; game_loop state

    | "look" -> inspect ()
    | "l" -> inspect ()

    | "inventory" -> inventory ()
    | "i" -> inventory ()

    | "action" -> action ()
    | "a" -> action ()

    | "use" -> use ()
    | "u" -> use ()

    | "north" -> move (0, -1)
    | "n" -> move (0, -1)
    | "south" -> move (0, 1)
    | "s" -> move (0, 1)
    | "east" -> move (-1, 0)
    | "e" -> move (-1, 0)
    | "west" -> move (1, 0)
    | "w" -> move (1, 0)

    | "quit" -> println "Thanks for playing!" ; ()

    | _ -> println "Invalid Command" ; game_loop state;;

println "Type 'help' for more info, 'quit' to quit";;
game_loop (new_state ())

