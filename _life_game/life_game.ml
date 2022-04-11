(*rules :
    dead cell with 3 neighbours => born
    alive cell with 2/3 neighbours => survive*)

type cell = {
  x : int;
  y : int;
  mutable state : bool;
  mutable nb_neighbour : int;
  mutable d : int
}

let size = 6 (* map size*size *)

let mk_cell x y = {x = x; y = y; state = false; nb_neighbour = 0; d = 0}

let map = Array.init size (fun y -> Array.init size (fun x -> mk_cell x y))

let switch arr i j = 
  arr.(i).(j).state <- Bool.not (arr.(i).(j).state);
  for k = i - 1 to i + 1 do
    for l = j - 1 to j + 1 do
      try 
        if (k < i) || (k == i && l < j) then
          arr.(k).(l).nb_neighbour <- arr.(k).(l).nb_neighbour + if arr.(i).(j).state then 1 else -1
        else if (k > i) || (k == i && l > j) then
          arr.(k).(l).d <- arr.(k).(l).d + if arr.(i).(j).state then 1 else -1
      with Invalid_argument a -> ()
    done
  done

let display arr = 
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if arr.(i).(j).state then Format.printf "!" else Format.printf "."
    done;
    Format.printf "\n"
  done;
  Format.printf "\n"


let init arr = 
  switch arr 2 1;
  switch arr 2 2;
  switch arr 2 3

let init arr =
  init arr; 
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let c = arr.(i).(j) in
      c.nb_neighbour <- c.nb_neighbour + c.d; c.d <- 0;
    done
  done

let next_step arr = 
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let c = arr.(i).(j) in
      if Bool.not c.state && c.nb_neighbour == 3 then switch arr i j else
      if c.state && (c.nb_neighbour != 2 && c.nb_neighbour != 3) then switch arr i j;
      c.nb_neighbour <- c.nb_neighbour + c.d; c.d <- 0;
    done
  done
