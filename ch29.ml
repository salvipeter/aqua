let input = 520185742
(* anything between 500000000 and 555555554 is the same *)

(* n-th level triangle numbers of 1..k *)
let rec sum n k =
    if n = 0 then
        [1]
    else
        let f k = sum (n - 1) k |> List.fold_left (+) 0 in
        Seq.ints 1 |> Seq.take k |> Seq.map f |> List.of_seq

let digits n =
    let rec f n acc =
        if n < 10 then
            n :: acc
        else
            f (n / 10) (n mod 10 :: acc)
    in f n []

(* Assumes that the input contains a digit less than its first digit *)
let answer =
    let ds = digits input in
    let n = List.length ds and k = List.hd ds - 1 in
    let f s x = s + List.fold_left (+) 0 (sum x 9) in
    let base = Seq.ints 0 |> Seq.take n |> Seq.fold_left f 0 in
    let rest = sum n 9 |> List.rev |> List.to_seq |> Seq.take k
               |> Seq.fold_left (+) 0 in
    base + rest
