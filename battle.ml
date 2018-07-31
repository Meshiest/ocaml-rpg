(* This file was created as an example of an interactive event
 * Everything in this file is self contained and modifications are very simple
 * It's almost like a separate game within the game
 * If a shopkeeper were to be implemented, this might be some useful code
 *)

open Types
open Utils
open Helpers

(* Combat types, this is an attacking or defending boolean *)
type move_type = Attack | Defend | Flee

(* Attack patterns are defined by finite lists or a function that determines the next move *)
and attack_pattern = Static of move_type list | Dynamic of (game_state -> int -> move_type)

(* A monster needs an attack pattern, hitpoints, and attack/defense in order to be meaningful in combat *)
and monster = {
  name : string; (* Monster's name *)
  pattern : attack_pattern; (* Monster's attack pattern *)
  hitpoints : int; (* Health *)
  atk : int; (* Attack damage *)
  def : int; (* Defense power *)
  can_flee : bool; (* Whether the player can escape this battle *)
}

(* A function to handle interactive battles with a monster, has its own input handling and returns a new state *)
let enter_battle (state: game_state) (monster: monster) (on_win: game_state -> game_state) (on_lose: game_state -> game_state) =
  println ("You have entered battle with '" ^ monster.name ^ "'");

  (* Input handling *)
  let rec get_player_move () : move_type =
    print_string "Enter a move (a/atk/d/def/f/flee): ";
    match read_line() with
      | "a" -> Attack
      | "atk" -> Attack
      | "d" -> Defend
      | "def" -> Defend
      | "f" -> Flee
      | "flee" -> Flee
      | _ -> println "Invalid move" ; get_player_move () in

  (* Monster move decision logic can be handled by the monster's patter *)
  let get_monster_move (monster: monster) (state: game_state) (turn: int) : move_type =
    match monster with
      | {pattern = Static moves; _} -> List.nth moves (turn mod List.length moves)
      | {pattern = Dynamic fn; _} -> fn state turn in

  (* Core battle logic *)
  let rec battle (state: game_state) (monster: monster) (turn: int) : game_state =
    (* Winning / losing decision based on hitpoints/health *)
    if state.health <= 0 then
      on_lose state
    else if monster.hitpoints <= 0 then
      on_win state
    else (
      println ("Turn " ^ string_of_int (turn + 1));
      println ("You have " ^ string_of_int state.health ^ " health");

      (* Get both parties' moves *)
      let player_move = get_player_move () in
      let monster_move = get_monster_move monster state turn in

      (* Handle choices *)
      match (player_move, monster_move) with
        | (Attack, Attack) ->
            println (
              "Both you and the monster attacked!\n" ^
              "You lost " ^ string_of_int monster.atk ^ " health\n" ^
              "'" ^ monster.name ^ "' lost " ^ string_of_int (get_player_attack state) ^ " health"
            );
            battle {
              state with health = state.health - monster.atk
            } {
              monster with hitpoints = monster.hitpoints - get_player_attack state
            } (turn + 1)
        | (Attack, Defend) ->
            println (
              "The monster defended!\n" ^
              "'" ^ monster.name ^ "' lost " ^ string_of_int (max (get_player_attack state - monster.def) 0) ^ " health"
            );
            battle state {
              monster with hitpoints = monster.hitpoints - max (get_player_attack state - monster.def) 0
            } (turn + 1)
        | (Defend, Attack) ->
            println (
              "The monster attacked!\n" ^
              "You lost " ^ string_of_int (max (monster.atk - get_player_defense state) 0) ^ " health"
            );
            battle {
              state with health = state.health - max (monster.atk - get_player_defense state) 0
            } monster (turn + 1)
        | (Defend, Defend) ->
            println "Both of you defended!\nNothing happened!";
            battle state monster (turn + 1)

        (* Both the monster can flee prematurely, the player's freedom depends on the monster *)
        | (Flee, _) ->
            if monster.can_flee then (
              println "You have fled...";
              on_lose state
            ) else (
              println "You cannot flee from this battle...";
              battle state monster turn
            )
        | (_, Flee) ->
            println "The monster has fled...";
            state
  ) in
  battle state monster 0
