open Board

let progress b =
  List.iter
    (fun a ->
      let open Ant in
      let prev_cell = a.loc in
      (match prev_cell.state with
      | Alive -> a.dir <- Direction.cw a.dir
      | Dead -> a.dir <- Direction.ccw a.dir);
      let new_coords = add_coords prev_cell.coords (Direction.to_delta a.dir) in
      a.loc <- find_cell b.grid new_coords;
      Cell.switch prev_cell)
    b.ants
