let input = 245701

let points =
    let base = Seq.ints 1 |> Seq.take 20 |> List.of_seq in
    let double = List.map (( * ) 2) base in
    let triple = List.map (( * ) 3) base in
    List.sort_uniq compare (base @ double @ triple @ [25; 50]) |> List.rev

(* Dynamic programming *)
let find_minimal n =
    let cache = Array.make (n + 1) 0 in
    let rec f n =
        if n = 0 || cache.(n) > 0 then
            cache.(n)
        else
            let result = List.filter (fun k -> k <= n) points
                         |> List.map (fun k -> f (n - k) + 1)
                         |> List.fold_left min max_int
            in cache.(n) <- result; result
    in Seq.ints 1 |> Seq.take n
       |> Seq.fold_left (fun x k -> x + f k) 0

let answer = find_minimal input
