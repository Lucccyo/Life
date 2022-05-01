module B = Board

let () =
  let b =
    Board.(
      create
        [ { x = 1; y = 1 } ]
        [
          { x = 1; y = 1 };
          { x = 6; y = 3 };
          { x = 7; y = 1 };
          { x = 4; y = 6 };
          { x = 6; y = 3 };
          { x = 2; y = 1 };
        ])
  in

  (*Fmt.epr "Startup board:\n";
    Fmt.epr "%a\n" Board.pp b;*)
  let prog_count = int_of_string Sys.argv.(1) in
  Fmt.pr "Going for %d steps\n" prog_count;

  for i = 0 to prog_count - 1 do
    Game.progress b;
    ignore i;
    (* if i mod 1_000_000 = 0 then*)
    (*Fmt.epr "Step %d: %a\n%!" i Board.pp b;*)
    Board.to_RLE b i (* Fmt.epr "Step %d: %a\n" i Board.pp b *)
  done;

  ()
