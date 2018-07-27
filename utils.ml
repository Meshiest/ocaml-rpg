(* Print line shortcut function *)
let println (str: string) = print_string (str ^ "\n")

(* Vector for storing positional information *)
type vec = {
  x: int;
  y: int;
}