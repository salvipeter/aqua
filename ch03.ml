(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = "LDRDLRDRDDRLRLDLLLUULURURLDULUUDRRDDRUDDRLRRULRDUDRUDRRLRDLDRULLDUUULDRRLDDLURLURRURLRLRUULDULDLLLUDLULDUUUDLDLLUUULDDLUURDUDDRULRULRULRDULRULULRLRDRDRULLRDRRRULLRDRDDRDULDDDUUDDRDRLRRRUUDDDULULLULURURLURULRDRUDLULRULLRRLLLRRRLRRLUULDUUUULLRDRRUULULURRURDRLDLLRUDULDRULDDRURLDRDLRRULRDRRUDRURULDURRULDLDULRLLLRLUURDLUUURUDLRLUUULULULUDRRDRUDLUDLRUUUDRRDDLLUDLDURDLRRRDRDLRLRRUDLRDRUUDULLDDRRUUDDRDRDLDRLLRRRUDLRDRUDDRURLLLDDLRRDUDDUDULURDLULDDLDRRRLLLRLDUURDUDULDDRRDRDLLDRDRRLLULLLRLURLLDDLDLRDUUUDR"

let inside (y, x) =
  y <= 2 && (x <= 2 && x + y >= 2 || x >= 3 && x - y <= 3) ||
  y >= 3 && (x <= 2 && y - x <= 3 || x >= 3 && x + y <= 8)

let step (y, x) c =
  let p = match c with
    | 'U' -> (y - 1, x)
    | 'D' -> (y + 1, x)
    | 'L' -> (y, x - 1)
    | 'R' -> (y, x + 1)
    |  _  -> failwith "invalid step"
  in if inside p then p else (y, x)

let answer =
  let f (pos, sum) c =
    let pos' = step pos c in
    (pos', sum + fst pos' + snd pos')
  in String.fold_left f ((0, 2), 0) input |> snd
