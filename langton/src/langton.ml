module B = Board

let () =
  (* let b = Board.(create [ { x = 0; y = 0 } ] []) in
   * Fmt.epr "%a\n%!" Board.pp b;
   *
   * let b = Board.(create [ { x = 0; y = -1 }; { x = 6; y = 6 } ] []) in
   * Fmt.epr "%a\n%!" Board.pp b; *)

  (* let b =
   *   Board.(
   *     create
   *       [ { x = 0; y = 0 }; { x = 6; y = 6 } ]
   *       [ { x = 3; y = 3 }; { x = 6; y = 6 } ])
   * in *)
  (* Fmt.epr "%a\n%!" Board.pp b; *)
  let b =
    Board.(create [ { x = 0; y = 0 }; { x = 6; y = 6 } ] [ { x = 3; y = 3 } ])
  in
  Fmt.epr "%a\n%!" Board.pp b;

  let prog_count = int_of_string Sys.argv.(1) in
  Fmt.epr "Going for %d progresses\n%!" prog_count;

  for i = 0 to prog_count - 1 do
    Game.progress b;
    Fmt.epr "Step %d: %a\n%!" i Board.pp b
  done;

  (* Fmt.epr "%a\n%!" Board.pp b;
   * Game.progress b;
   * Fmt.epr "%a\n%!" Board.pp b;
   * Game.progress b;
   * Fmt.epr "%a\n%!" Board.pp b; *)
  ()
