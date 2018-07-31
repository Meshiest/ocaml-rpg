(* All of the main interaction the player has with the game data is here
 * Any new commands like saving or loading would be added in here as they are core features
 * Handling checkpoints or lives would also be done in this file
 * A main menu, however, could be done outside of this file as a separate function
 *)
open Quadtree
open Types
open Utils
open Item
open Helpers

(* Recursive game loop where commands are entered and handled *)
let rec game_loop (state: game_state) : unit =
  if state.health <= 0 then
    println "You died... Game Over"
  else (
    print_string "> ";

    let input = read_line () in

    (* Helper for shifting a coord *)
    let offset ((x, y): int * int) : vec = {
      x = x + state.room.coord.x;
      y = y + state.room.coord.y;
    } in

    (* Shorthand lookup helper function *)
    let lookup_room (pos: vec) : room option =
      try quad_find state.map pos
      with Not_found -> None in

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
      let rec print_item_option (item: item option) : unit = match item with
        | Some item -> print_item item
        | None -> println "None"
        | Undecided fn -> print_item_option (fn state) in

      print_string ("   Health: " ^ string_of_int state.health ^
        "\n     Gold: " ^ string_of_int state.gold ^
        "\n   Weapon: ");
      print_item_option state.weapon;
      print_string "    Armor: ";
      print_item_option state.armor;
      println "Inventory: ";
      if state.inventory = [] then
        println "  Empty"
      else
        List.iter print_item state.inventory;
      game_loop state in

    (* Helper function for handling how items are used *)
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
            game_loop (equip_weapon (dequip_weapon state) {name; item_type = Weapon atk; count = 1})

        (* Handle using armor *)
        | Some {name; item_type = Armor def; _} ->
            println ("Equipping armor '" ^ name ^ "'");
            game_loop (equip_armor (dequip_weapon state) {name; item_type = Armor def; count = 1})

        (* Handle invalid search *)
        | None ->
            println "No item found";
            game_loop state

        (* Handle loot/items not programmed use cases for *)
        | _ ->
            println "No usable item found";
            game_loop state in

    (* Helper function for changing which room the player is in
     * Will automatically convert undecided options into some or none based on the current state *)
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
         use/u - Use an item (will prompt for item name)

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
      | "east" -> move (1, 0)
      | "e" -> move (1, 0)
      | "west" -> move (-1, 0)
      | "w" -> move (-1, 0)

      | "quit" -> println "Thanks for playing!" ; ()

      | _ -> println "Invalid Command" ; game_loop state
  );;
