(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = "LRUDDLRDLLDRUUUURLUDLLDLUDRURRDLUDRDURUURDLRULDULLRDRLLLDRDRRRLLDLRUUUDRLRDRLDRRUURDRLUDUUDUDLLDRULRLDRRLUUURRDDUDRDRURRLRRLLDRUUURLLRLRURRRUDUDURUDRURDRDDDURDLUDDLDUDRULDRULURLUULLLURDRLDUDRDUDRLDDRUULLLULRLDUURUUDRDLLDRRDRLLRUUURLDRULUDDRDDLDRURURR"

(*
       3             2
     2 1 5 6       3 1 4 6
       4             5

  Since the dice move together, we only need to keep track of one die,
  and sum the indices when the front face shows 1 or 6,
  and we don't need to know which is showing, so there are only 3 cases,
  depending on the 1/6 face is on top/bottom, left/right or front/back.
 *)

type die = TopBottom | LeftRight | FrontBack

let spin d = function
  | 'U' | 'D' -> (match d with
                  | TopBottom -> FrontBack
                  | LeftRight -> LeftRight
                  | FrontBack -> TopBottom)
  | 'L' | 'R' -> (match d with
                  | TopBottom -> TopBottom
                  | LeftRight -> FrontBack
                  | FrontBack -> LeftRight)
  | _         -> failwith "invalid spin"
                      
let answer =
  let f (die, index, sum) c =
    let die' = spin die c in
    (die', index + 1, if die' == FrontBack then sum + index + 1 else sum)
  in match String.fold_left f (FrontBack, -1, 0) input with
       (_, _, sum) -> sum
