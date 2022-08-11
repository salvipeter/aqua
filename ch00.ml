(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [(6,3);(4,2);(0,1);(7,4);(6,3);(0,1);(8,1);(4,2);(3,2);(9,3);(0,1);(4,2);(2,1);(8,3);(3,2);(0,1);(4,3);(6,2);(8,1);(3,2);(7,3);(6,2);(3,2);(8,1);(0,1);(6,3);(6,2);(0,1);(2,3);(6,3);(6,1);(7,1);(8,2);(8,1);(3,2);(7,3);(7,4);(0,1);(6,2);(6,3);(9,1)]

let chars = [|" "; ""; "abc"; "def"; "ghi"; "jkl"; "mno"; "pqrs"; "tuv"; "wxyz"|]

let digitToChar (d, k) = Array.get chars d |> Fun.flip String.get (k-1)

let answer = List.map digitToChar input |> List.to_seq |> String.of_seq
