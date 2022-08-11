(*
  Start the toplevel with:
    ocaml -nopromptcont -I +zarith zarith.cma zarith_top.cma

  - nopromptcont disables writing '*'s wen waiting for input
  - +zarith means <stdlib directory>/zarith
  - zarith_top.cma is included for nice output of bigints  
 *)

let input = [203217;151018;482359;782486;281651;721924;945710;131962;78308;661224]

let answer = List.map Z.of_int input |> List.fold_left Z.mul Z.one
