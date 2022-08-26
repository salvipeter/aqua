let input = "vepcundbyoaeirotivluxnotpstfnbwept"

let keyword = "power plant"

let grid =
    let alphabet = String.init 26 (fun n -> char_of_int (n + int_of_char 'a')) in
    let f xs x = if List.mem x xs || x = ' ' || x = 'j' then xs else x::xs in
    String.to_seq (keyword ^ alphabet) |> Seq.fold_left f [] |> List.rev

let pos c =
    let rec f x y = function
        | []    -> failwith "illegal character"
        | g::gs -> if x = 5 then
                       f 0 (y + 1) (g::gs)
                   else if g = c then
                       x, y
                   else
                       f (x + 1) y gs
    in f 0 0 grid

let rec get = function
    | -1, y -> get (4, y)
    | x, -1 -> get (x, 4)
    | x, y  -> List.nth grid (y * 5 + x)

let decode_pair (a, b) =
    let (ax, ay), (bx, by) = pos a, pos b in
    if ay = by then
        get (ax - 1, ay), get (bx - 1, by)
    else if ax = bx then
        get (ax, ay - 1), get (bx, by - 1)
    else
        get (bx, ay), get (ax, by)

let to_pairs str =
    let xs = String.to_seq str |> List.of_seq in
    let rec f = function
        | []       -> []
        | [_]      -> failwith "odd number of elements"
        | x::y::xs -> (x, y) :: f xs
    in f xs

let rec of_pairs = function
    | []           -> ""
    | (x, y) :: xs -> String.make 1 x ^ String.make 1 y ^ of_pairs xs

let answer = to_pairs input |> List.map decode_pair |> of_pairs
