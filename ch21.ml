#use "ch21-input.ml";;

let cleaner_size = 5

let row_width = 20

let choices = row_width - cleaner_size + 1

let motes row x = List.to_seq row
                  |> Seq.drop x 
                  |> Seq.take cleaner_size 
                  |> Seq.fold_left (+) 0

(* Dynamic programming *)
let most_motes rows =
    let cache = Array.make_matrix (List.length rows) choices (-1) in
    fun x ->
        let rec f k rows x =
            if x < 0 || x >= choices || rows = [] then 0
            else if cache.(k).(x) >= 0 then cache.(k).(x)
            else let row = List.hd rows in
                 let xs = [x - 1; x; x + 1] in
                 let n = motes row x in
                 let best = List.map (f (k + 1) (List.tl rows)) xs
                            |> List.fold_left max 0 |> (+) n
                 in cache.(k).(x) <- best; best
        in f 0 rows x

let answer =
    let f = most_motes input in
    Seq.ints 0 
    |> Seq.take choices
    |> Seq.map f
    |> Seq.fold_left max 0
