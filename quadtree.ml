open Utils

(* A binary tree but... in four directions *)
type 'a quad_node = {
  pos: vec;
  value: 'a option;
  north_east: 'a quad_node option;
  north_west: 'a quad_node option;
  south_east: 'a quad_node option;
  south_west: 'a quad_node option;
}

(* Error type *)
exception QuadError of string

let empty_node : 'a quad_node = {
  value = None;
  pos = {x = 0; y = 0};
  north_east = None;
  north_west = None;
  south_east = None;
  south_west = None;
}

(* Room quadtree lookup function *)
let rec quad_find (node: 'a quad_node) (loc: vec) : 'a =
  match node with
    (* Found the room with the same pos *)
    | {pos; value = Some value; _} when pos = loc -> value

    (* Find the room in another node *)
    | {pos; north_east = Some next; _} when loc.x < pos.x && loc.y < pos.y ->
        quad_find next loc
    | {pos; north_west = Some next; _} when loc.x >= pos.x && loc.y < pos.y ->
        quad_find next loc
    | {pos; south_east = Some next; _} when loc.x < pos.x && loc.y >= pos.y ->
        quad_find next loc
    | {pos; south_west = Some next; _} when loc.x >= pos.x && loc.y >= pos.y ->
        quad_find next loc

    | _ -> raise Not_found

(* Room quadtree add function *)
let rec quad_add ((loc, value): vec * 'a) (node: 'a quad_node) : 'a quad_node =
  (* Empty node *)
  let new_node = {
    value = Some value;
    pos = loc;
    north_east = None;
    north_west = None;
    south_east = None;
    south_west = None;
  } in

  match node with
    (* in the odd chance this is being used wrong, we'll handle it correctly anyway! *)
    | {pos; _} when pos = loc -> {node with value = Some value}

    (* Handle the different quadrant, create a new node if there is some room already here *)
    | {pos; north_east = Some next; _} when loc.x < pos.x && loc.y < pos.y ->
        {node with north_east = Some (quad_add (loc, value) next)}
    | {pos; north_east = None; _} when loc.x < pos.x && loc.y < pos.y ->
        {node with north_east = Some new_node}

    | {pos; north_west = Some next; _} when loc.x >= pos.x && loc.y < pos.y ->
        {node with north_west = Some (quad_add (loc, value) next)}
    | {pos; north_west = None; _} when loc.x >= pos.x && loc.y < pos.y ->
        {node with north_west = Some new_node}

    | {pos; south_east = Some next; _} when loc.x < pos.x && loc.y >= pos.y ->
        {node with south_east = Some (quad_add (loc, value) next)}
    | {pos; south_east = None; _} when loc.x < pos.x && loc.y >= pos.y ->
        {node with south_east = Some new_node}

    | {pos; south_west = Some next; _} when loc.x >= pos.x && loc.y >= pos.y ->
        {node with south_west = Some (quad_add (loc, value) next)}
    | {pos; south_west = None; _} when loc.x >= pos.x && loc.y >= pos.y ->
        {node with south_west = Some new_node}

    | _ -> raise (QuadError "Unreachable statement")
