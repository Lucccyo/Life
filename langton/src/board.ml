type coords = { x : int; y : int }

let add_coords a b = { x = a.x + b.x; y = a.y + b.y }

module Cell = struct
  type life = Alive | Dead
  type t = { coords : coords; mutable state : life }

  let create coords state = { coords; state }
  let switch c = c.state <- (if c.state = Alive then Dead else Alive)
end

module Direction = struct
  type t = R | B | L | T

  let cw = function R -> B | B -> L | L -> T | T -> R
  let ccw = function R -> T | T -> L | L -> B | B -> R

  let to_delta = function
    | R -> { x = 1; y = 0 }
    | B -> { x = 0; y = 1 }
    | L -> { x = -1; y = 0 }
    | T -> { x = 0; y = -1 }
end

module Ant = struct
  type t = { mutable loc : Cell.t; mutable dir : Direction.t }

  let create initial_cell = { loc = initial_cell; dir = Direction.L }
end

type cells = Cell.t list
type ants = Ant.t list

type t = {
  mutable left : int;
  mutable top : int;
  mutable width : int;
  mutable height : int;
  mutable grid : cells;
  ants : ants;
}

let find_cell grid coord =
  let rec aux = function
    | [] -> raise Not_found
    | hd :: tl -> if hd.Cell.coords = coord then hd else aux tl
  in
  aux grid

let size cell_cl ant_cl =
  let seq = List.to_seq (cell_cl @ ant_cl) in
  let seq_x = Seq.map (fun c -> c.x) seq in
  let seq_y = Seq.map (fun c -> c.y) seq in
  let min_x = Seq.fold_left min max_int seq_x in
  let max_x = Seq.fold_left max min_int seq_x in
  let min_y = Seq.fold_left min max_int seq_y in
  let max_y = Seq.fold_left max min_int seq_y in
  (min_x, min_y, max_x - min_x + 1, max_y - min_y + 1)

let create_grid minx width miny height cl =
  let cells = ref [] in
  for x = minx to minx + width - 1 do
    for y = miny to miny + height - 1 do
      let s = if List.mem { x; y } cl then Cell.Dead else Cell.Alive in
      let c = Cell.create { x; y } s in
      cells := c :: !cells
    done
  done;
  !cells

let create_ants cells cl = List.map (fun c -> Ant.create (find_cell cells c)) cl

let pp ppf t =
  Format.fprintf ppf "Board: w=%d, h=%d, #grid:%d, #ants:%d\n" t.width t.height
    (List.length t.grid) (List.length t.ants);

  for y = t.top to t.top + t.height - 1 do
    Format.fprintf ppf "%3d|" y;
    for x = t.left to t.left + t.width - 1 do
      let cell : Cell.t = find_cell t.grid { x; y } in

      let ant_opt : Ant.t option =
        List.find_opt
          (fun c ->
            let open Ant in
            c.loc.coords.x = x && c.loc.coords.y = y)
          t.ants
      in
      let color_prefix, color_suffix =
        match cell.state with
        | Cell.Dead -> ("\x1b[40;97;1m", "\x1b[0m")
        | Cell.Alive -> ("\x1b[107;30;1m", "\x1b[0m")
      in
      match ant_opt with
      | None -> Format.fprintf ppf " %s.%s" color_prefix color_suffix
      | Some { dir = Direction.L; _ } ->
          Format.fprintf ppf " %s<%s" color_prefix color_suffix
      | Some { dir = Direction.B; _ } ->
          Format.fprintf ppf " %sv%s" color_prefix color_suffix
      | Some { dir = Direction.R; _ } ->
          Format.fprintf ppf " %s>%s" color_prefix color_suffix
      | Some { dir = Direction.T; _ } ->
          Format.fprintf ppf " %s^%s" color_prefix color_suffix
    done;
    Format.fprintf ppf "\n"
  done

let create cl_grid cl_ant =
  let left, top, width, height = size cl_grid cl_ant in
  let grid = create_grid left width top height cl_grid in
  let ants = create_ants grid cl_ant in
  { left; top; width; height; grid; ants }

let padding_coords t =
  let left = t.left - 1 in
  let right = t.left + t.width in
  let top = t.top - 1 in
  let bot = t.top + t.height in

  let tl = { x = left; y = top } in
  let tr = { x = right; y = top } in
  let br = { x = right; y = bot } in
  let bl = { x = left; y = bot } in

  let delta_right = { x = 1; y = 0 } in
  let delta_bot = { x = 0; y = 1 } in
  let delta_left = { x = -1; y = 0 } in
  let delta_top = { x = 0; y = -1 } in

  let rec aux xy delta () =
    let continuation =
      let next_xy = add_coords xy delta in
      if next_xy = tr then aux next_xy delta_bot
      else if next_xy = br then aux next_xy delta_left
      else if next_xy = bl then aux next_xy delta_top
      else if next_xy = tl then fun () -> Seq.Nil
      else aux next_xy delta
    in
    Seq.Cons (xy, continuation)
  in
  aux tl delta_right

let padding_cells t =
  padding_coords t
  |> Seq.map (fun xy -> Cell.create xy Cell.Alive)
  |> List.of_seq

let grow t =
  t.grid <- padding_cells t @ t.grid;
  t.left <- t.left - 1;
  t.top <- t.top - 1;
  t.width <- t.width + 2;
  t.height <- t.height + 2
