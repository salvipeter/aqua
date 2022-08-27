let input =
    let f = open_in "snakes.txt" in
    let rec loop xs =
        try loop (input_line f :: xs)
        with End_of_file -> close_in f; xs
    in loop [] |> List.rev

let pos (x, y) = List.nth input y |> Fun.flip String.get x

let sum_snakes seq =
    let f (n, (k, v)) = function (* k: run length; v: current snake value *)
        | ' ' -> if k > 1 then (n + v * k, (0, 0)) else (n, (0, 0))
        | c   -> let d = int_of_char c - int_of_char 'a' + 1 in
                 (n, (k + 1, v + d))
    in Seq.fold_left f (0, (0, 0)) seq |> fst

let answer =
    let width = String.length (List.hd input) in
    let height = List.length input in
    let xs = Seq.ints 0 |> Seq.take width in
    let ys = Seq.ints 0 |> Seq.take height in
    let row y = Seq.map (fun x -> pos (x, y)) xs |> sum_snakes in
    let col x = Seq.map (fun y -> pos (x, y)) ys |> sum_snakes in
    let rows = Seq.map row ys |> Seq.fold_left (+) 0 in
    let cols = Seq.map col xs |> Seq.fold_left (+) 0 in
    rows + cols
