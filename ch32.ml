let input =
    let f = open_in "parens.txt" in
    let rec loop xs =
        try loop (input_line f :: xs)
        with End_of_file -> close_in f; xs
    in loop []

let opener = function
    | ')' -> '('
    | ']' -> '['
    | '}' -> '{'
    | _   -> failwith "invalid parenthesis"

exception BadParen

let check str =
    try
        let f xs c =
            match c with
            | '(' | '[' | '{' -> c :: xs
            | ')' | ']' | '}' ->
                    if xs <> [] && List.hd xs = opener c
                    then List.tl xs
                    else raise BadParen
            | _               -> xs
        in String.fold_left f [] str = []
    with BadParen -> false

let answer = List.filter check input |> List.length
