(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [(30,9,[(4,4)]);(45,10,[(9,9)]);(99,13,[(3,7)]);(204,22,[(0,4);(2,6);(9,19);(4,7)]);(429,37,[(14,3);(22,8);(18,4);(3,21)]);(940,45,[(23,2);(44,9)]);(1293,64,[(12,12);(31,2);(2,58);(17,4)]);(8487,101,[(0,0)]);(23853,101,[(10,4);(17,16);(91,20);(90,91)]);(97409,251,[(12,29);(8,237);(140,153)])]

let adjacent n (x,y) =
    let result = ref [] in
    if x > 0 then result := (x - 1, y) :: !result;
    if x < n - 1 then result := (x + 1, y) :: !result;
    if y > 0 then result := (x, y - 1) :: !result;
    if y < n - 1 then result := (x, y + 1) :: !result;
    !result

(* `remove x xs` returns (Some xs\x) or None if not found *)
let rec remove x = function
    | []    -> None
    | y::ys -> if x = y then Some ys
               else remove x ys |> Option.map (List.cons y)

(* `symdiff xs ys` is xs\ys U ys\xs *)
let rec symdiff xs = function
    | []    -> xs
    | y::ys -> match remove y xs with
               | None     -> y :: symdiff xs ys
               | Some xs' -> symdiff xs' ys

let rec play = function
    | 0, _, xs -> print_int (List.length xs); print_newline (); List.length xs
    | k, n, xs ->
            let xs' = List.map (adjacent n) xs |> List.fold_left symdiff [] in
            play (k - 1, n, xs')

let answer = List.map play input |> List.fold_left (+) 0
