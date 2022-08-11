(* vim: set nowrap: -*- truncate-lines: t -*- *)
 
let input = [(0,0,2,2);(15,0,18,2);(19,0,22,4);(1,1,5,5);(6,1,10,3);(11,1,13,3);(23,1,25,3);(4,2,7,4);(14,3,18,7);(1,4,3,10);(9,4,13,7);(7,5,8,6);(21,5,24,8);(0,6,2,9);(4,6,6,8);(17,6,20,10);(7,7,8,8);(9,8,11,11);(2,9,5,12);(12,9,14,11);(16,9,18,13);(19,9,22,10);(4,11,8,15);(19,11,24,13);(7,12,11,14);(10,12,12,16);(14,12,17,15);(0,13,2,16);(11,14,15,15);(16,14,22,19);(24,14,25,18);(9,15,11,19);(4,17,10,19);(12,17,15,18);(2,18,5,22);(17,18,19,25);(12,19,15,21);(9,20,11,22);(18,20,22,24);(23,20,25,24);(4,21,8,24);(13,22,16,24)]

let overlaps (lx1,ly1,ux1,uy1) (lx2,ly2,ux2,uy2) =
    lx1 < ux2 && ux1 > lx2 && ly1 < uy2 && uy1 > ly2

let squares (lx,ly,ux,uy) =
    let result = ref [] in
    for i = lx to ux - 1 do
        for j = ly to uy - 1 do
            result := (i, j) :: !result
        done
    done;
    !result

let answer =
    let goodTile t = List.exists (fun x -> x <> t && overlaps t x) input in
    let tiles = List.filter goodTile input in
    List.map squares tiles |> List.concat |> List.sort_uniq compare |> List.length 
