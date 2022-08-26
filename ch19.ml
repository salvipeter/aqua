(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [(30,9,[(4,4)]);(45,10,[(9,9)]);(99,13,[(3,7)]);(204,22,[(0,4);(2,6);(9,19);(4,7)]);(429,37,[(14,3);(22,8);(18,4);(3,21)]);(940,45,[(23,2);(44,9)]);(1293,64,[(12,12);(31,2);(2,58);(17,4)]);(8487,101,[(0,0)]);(23853,101,[(10,4);(17,16);(91,20);(90,91)]);(97409,251,[(12,29);(8,237);(140,153)])]

(* Note that the result is sorted *)
let adjacent n (x,y) =
    let result = ref [] in
    if x < n - 1 then result := (x + 1, y) :: !result;
    if y < n - 1 then result := (x, y + 1) :: !result;
    if y > 0 then result := (x, y - 1) :: !result;
    if x > 0 then result := (x - 1, y) :: !result;
    !result

(* Assumes that the lists are sorted *)
let rec symdiff xs ys = match xs, ys with
    | [],    ys    -> ys
    | xs,    []    -> xs
    | x::xs, y::ys -> let c = compare x y in
                      if c < 0 then
                          x :: symdiff xs (y::ys)
                      else if c > 0 then
                          y :: symdiff (x::xs) ys
                      else
                          symdiff xs ys

let position x xs =
    let rec f k = function
        | []    -> raise Not_found
        | y::ys -> if x = y then k else f (k + 1) ys
    in f 0 xs

(* Like List.length, but also prints the result *)
let print_length xs =
    let n = List.length xs in
    print_int n; print_newline ();
    n

let play (k, n, xs) =
    let rec f cache xs = function
        | 0 -> print_length xs
        | k -> if List.mem xs cache then
            let cache = List.rev cache in
            let index = position xs cache in
            let cycle = List.length cache - index in
            print_string "cycle: "; print_int cycle; print_string " -> ";
            List.nth cache (index + k mod cycle) |> print_length
        else
            let xs' = List.map (adjacent n) xs |> List.fold_left symdiff [] in
            f (xs :: cache) xs' (k - 1)
    in f [] xs k

(* Still a bit slow (2m 30s) *)
let answer = List.map play input |> List.fold_left (+) 0
