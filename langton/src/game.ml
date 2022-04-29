open Board

let progress b =
  (* Step 1. Turn and calc new coords *)
  let new_coords =
    List.map
      (fun a ->
        let open Ant in
        let prev_cell = a.loc in
        (match prev_cell.state with
        | Alive -> a.dir <- Direction.cw a.dir
        | Dead -> a.dir <- Direction.ccw a.dir);
        add_coords prev_cell.coords (Direction.to_delta a.dir))
      b.ants
  in

  (* Step 2. Grow if needed *)
  let should_grow =
    List.exists
      (fun c ->
           c.x < b.left
        || c.x >= b.left + b.width
        || c.y < b.top
        || c.y >= b.top + b.height)
      new_coords
  in
  if should_grow then (
    Fmt.epr "Let's grow board\n";
    Board.grow b;
    Fmt.epr ">>>>>>>>>>>>>>> Grown board:\n";
    Fmt.epr "%a\n" Board.pp b;
    Fmt.epr ">>>>>>>>>>>>>>>\n";

  );

  (* Step 3. Move and flip *)
  List.iter2
    (fun (a : Ant.t) new_coords ->
      let prev_cell = a.loc in
      a.loc <- find_cell b.grid new_coords;
      Cell.switch prev_cell)
    b.ants new_coords;
  ()
