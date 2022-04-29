module B = Board

let () =
  let b =
    Board.(create [ { x = 0; y = 0 }; { x = 6; y = 6 } ] [ { x = 3; y = 3 } ])
  in
  Fmt.epr "Startup board:\n";
  Fmt.epr "%a\n" Board.pp b;

  let prog_count = int_of_string Sys.argv.(1) in
  Fmt.epr "Going for %d steps\n" prog_count;

  for i = 0 to prog_count - 1 do
    Game.progress b;
    ignore i;
    Fmt.epr "Step %d: %a\n" i Board.pp b
  done;

  ()
