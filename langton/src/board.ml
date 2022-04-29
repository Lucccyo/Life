type coords = { x : int; y : int }

module Cell = struct
  type life = Alive | Dead
  type t = { coords : coords; mutable state : life }

  let create coords state = { coords; state }
  let switch c = c.state <- (if c.state = Alive then Dead else Alive)
end

module Ant = struct
  type t = { mutable loc : Cell.t }

  let create initial_cell = { loc = initial_cell }
  let move t target_cell = t.loc <- target_cell
end

type cells = Cell.t list
type ants = Ant.t list

type t = {
  mutable width : int;
  mutable height : int;
  mutable grid : cells;
  mutable ants : ants;
}

let rec find cl coords =
  match cl with
  | [] -> raise Not_found
  | hd :: tl -> if hd.Cell.coords == coords then hd else find tl coords

let rec size_x cl max min =
  let min = ref min in
  let max = ref max in
  match cl with
  | [] -> !max - !min
  | hd :: tl ->
      if hd.x < !min then min := hd.x;
      if hd.x > !max then max := hd.x;
      size_x tl !max !min

let rec size_y cl max min =
  let min = ref min in
  let max = ref max in
  match cl with
  | [] -> !max - !min
  | hd :: tl ->
      if hd.y < !min then min := hd.y;
      if hd.y > !max then max := hd.y;
      size_y tl !max !min

let set_board x y cl =
  let cells = ref [] in
  for x = 0 to x do
    for y = 0 to y do
      let s = if List.mem { x; y } cl then Cell.Alive else Cell.Dead in
      let c = Cell.create { x; y } s in
      cells := !cells @ [ c ]
    done
  done;
  !cells

let rec set_ants cells cl ants =
  match cl with
  | [] -> ants
  | hd :: tl -> (
      try
        let c = find cells hd in
        set_ants cells tl (ants @ [ Ant.create c ])
      with Not_found -> set_ants cells tl ants)

let create cl_grid cl_ant =
  let width = size_x cl_grid max_int min_int in
  let height = size_y cl_grid max_int min_int in
  let grid = set_board width height cl_grid in
  { width; height; grid; ants = set_ants grid cl_ant [] }
