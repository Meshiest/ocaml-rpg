(* Everything here is general helper functions and types that do not really pertain to this project specifically
 * Nothing in here really deserves its own module (the quadtree does)
 *)

(* Print line shortcut function *)
let println (str: string) = print_string (str ^ "\n")

(* Vector for storing positional information *)
type vec = {
  x: int;
  y: int;
}
