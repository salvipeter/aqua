let input =
  let f = open_in "rubik.txt" in
  let line = input_line f in
  close_in f;
  line

type rotation = F | L | R | U | D | B

let rotations str =
    let f xs = function
        | 'F'  -> (F, true) :: xs
        | 'L'  -> (L, true) :: xs
        | 'R'  -> (R, true) :: xs
        | 'U'  -> (U, true) :: xs
        | 'D'  -> (D, true) :: xs
        | 'B'  -> (B, true) :: xs
        | '\'' -> (match xs with
                   | (r, true)::xs -> (r, false) :: xs
                   | _             -> failwith "invalid apostrophe")
        | _    -> failwith "invalid character"
    in String.fold_left f [] str |> List.rev

(* The 20 movable cubes are indexed in x,y,z-order. *)
let cubes = function
    | F -> [ 0; 3; 5; 6; 7; 4; 2; 1]
    | L -> [ 0; 8;12;15;17;10; 5; 3]
    | R -> [ 2; 4; 7;11;19;16;14; 9]
    | U -> [ 5;10;17;18;19;11; 7; 6]
    | D -> [ 0; 1; 2; 9;14;13;12; 8]
    | B -> [12;13;14;16;19;18;17;15]

(* A cube is identified by what color is on its x,y,z sides *)
let init = [(3,5,1);(0,5,1);(4,5,1);(3,0,1);(4,0,1);
            (3,2,1);(0,2,1);(4,2,1);(3,5,0);(4,5,0);
            (3,2,0);(4,2,0);(3,5,6);(0,5,6);(4,5,6);
            (3,0,6);(4,0,6);(3,2,6);(0,2,6);(4,2,6)]

let turn_cube (x, y, z) = function
    | F | B -> (y, x, z)
    | L | R -> (x, z, y)
    | U | D -> (z, y, x)

let rotate_list = function
    | []    -> []
    | x::xs -> xs @ [x]

let rotate rubik (face, cw) =
    let ns = if cw then cubes face else List.rev (cubes face) in
    let ns' = rotate_list (rotate_list ns) in
    let turned = List.map (fun n -> turn_cube rubik.(n) face) ns in
    List.iter2 (fun c n -> rubik.(n) <- c) turned ns';
    rubik

let front_product rubik =
    let f n k = let (_, _, z) = rubik.(k) in z * n in
    List.fold_left f 1 (cubes F)

let answer =
    List.fold_left rotate (Array.of_list init) (rotations input) |> front_product
