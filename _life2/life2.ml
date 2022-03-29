type cell = {
  state : bool;
  nb_neighbour : int
}

let null_cell = {state = false ; nb_neighbour = 0}

let find_cell ho hn x y = 
  try
    Hashtbl.find hn (x, y)
  with Not_found ->
    begin try
      Hashtbl.find ho (x, y)
    with Not_found ->
      null_cell
    end

let xmin = ref 0
let xmax = ref 0
let ymin = ref 0
let ymax = ref 0

let display ho = 
  for i = !xmin to !xmax do
    for j = !ymin to !ymax do
      let c = find_cell ho ho i j in
      if c.state then Format.printf "!" else Format.printf "."
    done;
    Format.printf "\n"
  done;
  Format.printf "\n"

  let display_nb ho = 
    for i = !xmin to !xmax do
      for j = !ymin to !ymax do
        let c = find_cell ho ho i j in
        Format.printf "%d" c.nb_neighbour
      done;
      Format.printf "\n"
    done;
    Format.printf "\n"
  
let set_cell hn i j c = 
  (*if c.state || c.nb_neighbour > 0 then*) begin
    Format.printf "SET: %d %d -> %c %d\n" i j (if c.state then '!' else '.') c.nb_neighbour;
    if i < !xmin then xmin := i;
    if i > !xmax then xmax := i;
    if j < !ymin then ymin := j;
    if j > !ymax then ymax := j;
    Hashtbl.replace hn (i, j) c
  end  



let switch ho hn i j = 
  let c = find_cell ho hn i j in
  let c = {c with state = not c.state} in
  Format.printf "SWT: %d %d -> %c\n" i j (if c.state then '!' else '.');
  set_cell hn i j c;
  for k = i - 1 to i + 1 do
    for l = j - 1 to j + 1 do 
      if k != i || l != j then
        let d = find_cell ho hn k l in
        let d = {d with nb_neighbour = d.nb_neighbour + if c.state then 1 else -1} in
        set_cell hn k l d
    done
  done

let next_step ho hn = 
  let update_cell (i, j) c = 
    if (not c.state) && c.nb_neighbour == 3 then switch ho hn i j else
    if c.state && (c.nb_neighbour != 2 && c.nb_neighbour != 3) then switch ho hn i j 
  in
  Hashtbl.iter update_cell ho 


let rec simulate ho n =
  if n > 0 then begin 
    let hn = Hashtbl.create (Hashtbl.length ho / 2) in
    next_step ho hn;
    display hn;
    simulate hn (n - 1)
  end else ho

let ho = Hashtbl.create 17

let init = 
  switch ho ho 0 (-1);
  switch ho ho 0 0;
  switch ho ho 0 1;
  switch ho ho 1 (-1);
  switch ho ho 1 1;
  switch ho ho 2 (-1);
  switch ho ho 2 1