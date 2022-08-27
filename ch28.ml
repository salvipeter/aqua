(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [{|               \ /       /\          |};
             {|   /         \/   / \           \  / |};
             {|         / \       /  /     /       /|};
             {|   /      \/      \        /\      \ |};
             {|           /  /         / \  \ \   / |};
             {|    /       \   \  /    \       \\   |};
             {|      \\   \    / \      \      \    |};
             {|    \     /     \    \/  \   \  \\   |};
             {|           \    /     /      \       |};
             {|         \                 /        \|};
             {|  \    \  \    \    \   \            |};
             {|          \             /\\  \       |};
             {| //  \      /        \      //       |};
             {|  \\      /       /    \       \/  / |};
             {|/  / /             /  \   \/      /  |};
             {|     /  \\\ \   \  \\   /  / /       |};
             {|        /    /                 /     |};
             {|      \/ /  \/ //   \      \      /  |};
             {|  \ //      \      /     \         /\|};
             {|                  \       \      \   |};
             {|     \/     \\/  /         /         |};
             {|     / //         \         /   /  / |};
             {|         /        /  \   \ //\       |};
             {|                 \        \          |};
             {|         \   // \ /                  |};
             {|   /  \       \ /   \  \/     /      |};
             {| \   /    \              \           |};
             {|      \\      /  /            /      |};
             {|    \ /      /        \      \       |};
             {|\\   \  \   \            /\  /   /   |};
             {|  /     / \  \        \/             |};
             {|     //\  \  /    \   / \   \     /  |};
             {|     \ \  /         /     /       \  |};
             {|/      \   /  \       /  / /    \   \|};
             {|            /      \  \\  // /      \|};
             {|         \     /            \ \/ / \ |};
             {| //  \ \   \    \     \      \       |}]

let characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

let text = "FISSION_MAILED"

let of_index i = String.get characters i
let to_index c = String.index characters c

let init xs =
    let n = List.length xs in
    let row k = List.nth xs k |> String.to_seq |> Array.of_seq in
    Array.init n row

type directions = Left | Right | Up | Down

let turn dir c =
    match dir, c with
    | Left,  '/'  -> Down,  '\\'
    | Right, '/'  -> Up,    '\\'
    | Up,    '/'  -> Right, '\\'
    | Down,  '/'  -> Left,  '\\'
    | Left,  '\\' -> Up,    '/'
    | Right, '\\' -> Down,  '/'
    | Up,    '\\' -> Left,  '/'
    | Down,  '\\' -> Right, '/'
    | _           -> dir,   ' '

let step (x, y) = function
    | Left  -> (x - 1, y)
    | Right -> (x + 1, y)
    | Up    -> (x, y - 1)
    | Down  -> (x, y + 1)

let decode mirrors c =
    let n = Array.length mirrors in
    let check (x, y) =
        if x < 0 || x >= n then
            Some (of_index y)
        else if y < 0 || y >= n then
            Some (of_index x)
        else
            None
    in let rec f pos dir =
        match check pos with
        | Some c -> mirrors, c
        | None ->
                let x, y = pos in
                let c = mirrors.(y).(x) in
                let dir', c' = turn dir c in
                mirrors.(y).(x) <- c';
                f (step pos dir') dir'
    in f (0, to_index c) Right

let answer =
    let f (mirrors, xs) c = let mirrors', c' = decode mirrors c in
                            (mirrors', c'::xs)
    in String.fold_left f (init input, []) text |> snd |> List.rev
       |> List.to_seq |> String.of_seq
