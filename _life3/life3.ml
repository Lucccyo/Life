type cell = {
  state : bool;
  nb_neighbour : int
}

let null_cell = {state = false ; nb_neighbour = 0}

let get_cell hc p =
  try
    Hashtbl.find hc p
  with Not_found ->
    null_cell

let set_cell hc p nnb nst =
  if not nst && nnb == 0 then Hashtbl.remove hc p else
  let c = {state = nst ; nb_neighbour = nnb} in
  Hashtbl.replace hc p c

let switch nhd (i, j) nst = 
  for k = i - 1 to i + 1 do
    for l = j - 1 to j + 1 do 
      let d = try Hashtbl.find nhd (k, l) with Not_found -> 0 in
      let d = d + if k == i && l == j then 0 else if nst then 1 else -1 in
      Hashtbl.replace nhd (k, l) d
    done
  done

let new_state ohd hc nhd = 
  Hashtbl.iter (fun p od -> 
    let oc = get_cell hc p in
    let nnb = oc.nb_neighbour + od in
    let nst = (nnb == 2 && oc.state) || nnb == 3 in
    if oc.state != nst then begin
      switch nhd p nst;
      set_cell hc p nnb nst
    end
    else if oc.nb_neighbour != nnb then 
      set_cell hc p nnb nst
  ) ohd

  let to_RLE hc = 
    let xmin = ref max_int in
    let xmax = ref min_int in
    let ymax = ref min_int in
    let ymin = ref max_int in
    Hashtbl.iter (fun (i, j) c ->
      if i < !xmin then xmin := i;
      if i > !xmax then xmax := i;
      if j < !ymin then ymin := j;
      if j > !ymax then ymax := j;   
    ) hc;
    let hcf = ref "" in
    for i = !xmin to !xmax do
      for j = !ymin to !ymax do
        let c = get_cell hc (i,j) in
        hcf := !hcf ^ begin if c.state then "o" else "b" end
      done;
      hcf := !hcf ^ "$"
    done;
    hcf := !hcf ^ "!";
    !hcf

let display hc = 
  let xmin = ref max_int in
  let xmax = ref min_int in
  let ymax = ref min_int in
  let ymin = ref max_int in
  Hashtbl.iter (fun (i, j) c ->
    if i < !xmin then xmin := i;
    if i > !xmax then xmax := i;
    if j < !ymin then ymin := j;
    if j > !ymax then ymax := j;   
  ) hc;
  for i = !xmin to !xmax do
    for j = !ymin to !ymax do
      let c = get_cell hc (i, j) in
      if c.state then Format.printf "!" else Format.printf "."
    done;
    Format.printf "\n"
  done;
  Format.printf "\n"

let rec simulate ohd hc nhd n =
  display hc;
  Format.printf "%s\n" (to_RLE hc);
  if n <= 0 then ohd else begin
    new_state ohd hc nhd;
    Hashtbl.clear ohd;
    simulate nhd hc ohd (n-1)
  end

let init_cell hc hd i j =
  switch hd (i, j) true;
  set_cell hc (i, j) 0 true

let hc = Hashtbl.create 15
let hd = Hashtbl.create 15

let init = 
  init_cell hc hd 0 (-1);
  init_cell hc hd 0 0;
  init_cell hc hd 0 1;
  init_cell hc hd 1 (-1);
  init_cell hc hd 1 1;
  init_cell hc hd 2 (-1);
  init_cell hc hd 2 1