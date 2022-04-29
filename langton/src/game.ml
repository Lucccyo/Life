open Board

let progress b =
  List.iter
    (fun a ->
      let open Ant in
      Cell.switch a.loc;
      match a.loc.state with
      | Alive -> a.dir <- Direction.cw a.dir
      | Dead ->
          a.dir <- Direction.ccw a.dir;
          let new_coords = add_coords a.loc.coords (Direction.to_delta a.dir) in
          let c = find_cell b.grid new_coords in
          a.loc <- c)
    b.ants
