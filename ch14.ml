#use "ch14-input.ml";;

let wins = [[ 6;17;34;50;68];
            [10;21;45;53;66];
            [ 5;25;36;52;69];
            [14;30;33;54;63];
            [15;23;41;51;62];
            [ 6;10; 5;14;15];
            [17;21;25;30;23];
            [34;45;36;33;41];
            [50;53;52;54;51];
            [68;66;69;63;62];
            [ 6;21;36;54;62];
            [68;53;36;30;15]]

let rec removeHit x = function
    | []    -> []
    | y::xs -> if x == y then removeHit x xs
               else y :: removeHit x xs

let bingo xs =
    let rec f k nums = function
        | x::xs -> let nums' = List.map (removeHit x) nums in
                   if List.exists (fun x -> x = []) nums'
                   then k
                   else f (k + 1) nums' xs
        | []    -> failwith "cannot win"
    in f 1 wins xs

let answer = List.map bingo input |> List.fold_left (+) 0 
