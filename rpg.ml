
type 'a option = Some of 'a | None

(* A room in the dungeon *)
type room = {
  north: room option;
  east: room option;
  south: room option;
  west: room option;
  title: string; (* Short description of the room *)
  action: (game_state -> game_state); (* Interaction function for the room, use `no_action` for no action *)
}

(* Current state of the player *)
and game_state = {
  room: room;
  gold: int;
  health: int;
}

(* Function to be used when a room has no action *)
let no_action state: game_state =
  print_string "Nothing to do here..."; 
  state

(* Default room the player always starts in *)
let starting_room = {
  north = None;
  south = None;
  east = None;
  west = None;
  title = "Starting Room";
  action = no_action;
}

(* Initial game state for starting with nothing *)
let new_state () = {
  room = starting_room;
  gold = 0;
  health = 100;
}

(* Recursive game loop where commands are entered and handled *)
let rec game_loop state: game_state = 
  let input = read_line() in

  let inspect () =
    print_string ("You are in '" ^ state.room.title ^ "'\n") ; game_loop state in

  match input with
    | "help" -> print_string "Commands:
       help - This help message
  inspect/i - A brief description of the room
       quit - Closes the game\n" ; game_loop state

    | "inspect" -> inspect()
    | "i" -> inspect()

    | "quit" -> print_string "Thanks for playing!\n" ; new_state ()

    | _ -> print_string "Invalid Command\n" ; game_loop state;;

print_string "Type 'help' for more info, 'quit' to quit\n";;
game_loop (new_state ())

