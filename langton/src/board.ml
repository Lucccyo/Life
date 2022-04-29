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
      let s = if List.mem { x; y } cl then Cell.Alive else Cell.Dead in
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
      match (cell.state, ant_opt) with
      | Cell.Alive, None -> Format.fprintf ppf "%%"
      | Dead, None -> Format.fprintf ppf "."
      | Alive, Some _ -> Format.fprintf ppf "A"
      | Dead, Some _ -> Format.fprintf ppf "a"
    done;
    Format.fprintf ppf "\n"
  done

let create cl_grid cl_ant =
  let left, top, width, height = size cl_grid cl_ant in
  let grid = create_grid left width top height cl_grid in
  let ants = create_ants grid cl_ant in
  { left; top; width; height; grid; ants }
