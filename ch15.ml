(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [("dog","war"); ("bow","ply"); ("tree","fled"); ("fire","park"); ("forge","house"); ("stall","chili"); ("start","great"); ("inner","outer"); ("asking","bobble"); ("coffee","drawer")]

let dictionary = 
    let result = Array.make 30 [] in (* there are no words >=30 chars long *)
    let f = open_in "words.txt" in
    let rec loop () =
        let line = try Some (input_line f |> String.trim)
                   with End_of_file -> close_in f; None
        in match line with
          | Some line -> let n = String.length line in
                         Array.set result n (line :: Array.get result n);
                         loop () 
          | None      -> ()
    in loop (); result

let oneDistance a b =
    let n = String.length a in
    let rec f d k =
       match (d, k) with
       | (2, _)  -> false
       | (1, -1) -> true
       | (_, -1) -> false
       | (d, k)  -> if String.get a k = String.get b k
                    then f d (k - 1)
                    else f (d + 1) (k - 1)
    in f 0 (n - 1)

(* slow, but still well under 1 minute compiled *)
let dijkstra (a, b) =
    let n = String.length a in
    let words = Array.get dictionary n in
    let best = ref None in
    let rec f dists = function
        | []    -> List.assoc b dists
        | x::xs -> let d = List.assoc x dists in
                   if x = b then best := Some d;
                   if Option.is_some !best && Option.get !best <= d then f dists xs else
                   let adjacent = List.filter (oneDistance x) words in
                   let xs = List.filter (fun y -> x <> y) xs in
                   let g (dists, xs) y =
                       match List.assoc_opt y dists with
                       | None    -> ((y, d + 1) :: dists, xs @ [y])
                       | Some d' -> if d + 1 < d'
                                    then ((y, d + 1) :: List.remove_assoc y dists, xs @ [y])
                                    else (dists, xs)
                   in let (dists', xs') = List.fold_left g (dists, xs) adjacent in
                   f dists' xs'
    in let result = f [(a, 0)] [a] + 1 in
       print_int result;
       print_endline (" " ^ a ^ " - " ^ b);
       result 

let answer = List.map dijkstra input |> List.fold_left ( * ) 1
