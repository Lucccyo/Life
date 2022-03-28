(*rules :
    dead cell with 3 neighbours => born
    alive cell with 2/3 neighbours => survive*)

type cell = {
  mutable state : bool;
  mutable nb_neighbour : int
  }

let a0 = {state = false ; nb_neighbour = 0}
let a1 = {state = false ; nb_neighbour = 0}
let a2 = {state = false ; nb_neighbour = 0}
let b0 = {state = false ; nb_neighbour = 0}
let b1 = {state = false ; nb_neighbour = 0}
let b2 = {state = false ; nb_neighbour = 0}
let c0 = {state = false ; nb_neighbour = 0}
let c1 = {state = false ; nb_neighbour = 0}
let c2 = {state = false ; nb_neighbour = 0}

let arr = [| [|a0 ; a1 ; a2|] ; [|b0 ; b1 ; b2|] ; [|c0 ; c1 ; c2|] |]





let size = 3 (* map size*size *)

let map = Array.make size (Array.make size {state = false ; nb_neighbour = 0})

let switch arr i j = 
  arr.(i).(j).state <- Bool.not (arr.(i).(j).state);
  for k = i - 1 to i + 1 do
    for l = j - 1 to j + 1 do
      if (k != i) || (l != j) then begin
        try
          arr.(k).(l).nb_neighbour <- arr.(k).(l).nb_neighbour + if arr.(i).(j).state then 1 else -1
        with Invalid_argument a ->
          Format.printf ":) "
      end
    done
  done;
  Format.printf "\n"

let display arr = 
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if arr.(i).(j).state then Format.printf "!" else Format.printf "."
    done;
    Format.printf "\n"
  done;
  Format.printf "\n"

let next_step arr = 
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let c = arr.(i).(j) in
      if Bool.not c.state && c.nb_neighbour == 3 then switch arr i j;
      if c.state && (c.nb_neighbour != 2 && c.nb_neighbour != 3) then switch arr i j
    done
  done

let () = 
  switch arr 0 0;
  switch arr 1 0;
  switch arr 1 1;
  display arr;
  Format.printf "\n";
  next_step arr;
  display arr