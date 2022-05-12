type coords = { x : int; y : int }
type life = Alive | Dead
type cell = { coords : coords; mutable state : life }
type direction = Right | Bottom | Left | Top
type ant = { mutable loc : cell; mutable dir : direction }

type board = private {
  mutable left : int;
  mutable top : int;
  mutable width : int;
  mutable height : int;
  mutable grid : cell list;
  ants : ant list;
}
(*@ (* Ensures that [width] and [height] are strictly positive *)
    invariant (width > 0) && (height > 0)

    (* Ensures that all cells form a grid *)
    invariant width * height = List.length grid
    invariant
      forall i. top  <= i <  top + height ->
      forall j. left <= j < left + width  ->
        List._exists (fun cell -> cell.coords.x = j /\ cell.coords.y = i) grid

    (* Ensures that cells pointed by ants are in [grid] *)
    invariant List.for_all (fun ant ->
      List._exists (fun cell -> ant.loc = cell) grid) ants *)

val progress : board -> unit
(*@ progress b

    modifies b

    (* Ensures that the grid did not changed or was dilated by 1 *)
    ensures (old b.left = b.left) \/ (old b.left = b.left - 1)
    ensures (old b.top = b.top) \/ (old b.top = b.top - 1)
    ensures (old b.height = b.height) \/ (old b.height = b.height + 2)
    ensures (old b.width = b.width) \/ (old b.width = b.width + 2)

    (* Ensures that all ants properly moved *)
    ensures
      List.for_all2
        (fun old_ant new_ant ->
          let delta_x = abs (old_ant.loc.coords.x - new_ant.loc.coords.x) in
          let delta_y = abs (old_ant.loc.coords.y - new_ant.loc.coords.y) in
          logxor delta_x delta_y = 1)
        (old b.ants) b.ants *)
