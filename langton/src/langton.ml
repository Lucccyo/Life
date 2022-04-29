module B = Board

let () =
  let b = Board.(create [ { x = 0; y = 0 } ] []) in
  Fmt.epr "%a\n%!" Board.pp b;
  let b = Board.(create [ { x = 0; y = 0 }; { x = 6; y = 6 } ] []) in
  Fmt.epr "%a\n%!" Board.pp b;
  let b =
    Board.(create [ { x = 0; y = 0 }; { x = 6; y = 6 } ] [ { x = 3; y = 3 } ])
  in
  Fmt.epr "%a\n%!" Board.pp b;
  let b =
    Board.(
      create
        [ { x = 0; y = 0 }; { x = 6; y = 6 } ]
        [ { x = 3; y = 3 }; { x = 6; y = 6 } ])
  in
  Fmt.epr "%a\n%!" Board.pp b
