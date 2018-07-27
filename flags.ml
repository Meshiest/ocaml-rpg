open Types

(* Add a flag to the state *)
let add_flag (state: game_state) (flag: string) : game_state = {
  state with flags = flag::state.flags
}

(* Determine if the state has a specific flag *)
let has_flag ({flags; _}: game_state) (flag: string) : bool =
  List.exists ((=)flag) flags

(* Remove a flag only upon the first encounter *)
let remove_flag (state: game_state) (flag: string) : game_state = {
  state with flags =
    let rec find = function
      | x::xs -> if x = flag then xs else x::find xs
      | [] -> []
    in find state.flags
}

(* Remove every instance of a flag *)
let remove_flags (state: game_state) (flag: string) : game_state = {
  state with flags = List.filter ((!=)flag) state.flags
}
