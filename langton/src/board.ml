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
  ants : ants;
}

let rec find cl coords =
  match cl with
  | [] -> raise Not_found
  | hd :: tl -> if hd.Cell.coords == coords then hd else find tl coords

let size cell_cl ant_cl =
  let seq = List.to_seq (cell_cl @ ant_cl) in
  let seq_x = Seq.map (fun c -> c.x) seq in
  let seq_y = Seq.map (fun c -> c.y) seq in
  let min_x = Seq.fold_left min max_int seq_x in
  let max_x = Seq.fold_left max min_int seq_x in
  let min_y = Seq.fold_left min max_int seq_y in
  let max_y = Seq.fold_left max min_int seq_y in
  (max_x - min_x, max_y - min_y)

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
  let width, height = size cl_grid cl_ant in
  let grid = set_board width height cl_grid in
  { width; height; grid; ants = set_ants grid cl_ant [] }

(*accepter les fourmis partout*)
