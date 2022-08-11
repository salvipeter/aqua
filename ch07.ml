(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [("Lee","Michael",7,11);("Michael","James",9,11);("Sam","Lee",11,3);("Michael","Sam",3,11);("James","Lee",11,4);("Sam","James",9,11);("Jordan","otherRyan",1,11);("Gary","Sean",8,11);("Sean","Jordan",11,2);("Jordan","Gary",3,11);("Sean","Ciaran",11,3);("Ciaran","Jordan",2,11);("Ciaran","Gary",0,11);("otherRyan","Sean",12,10);("Gary","otherRyan",6,11);("otherRyan","Ciaran",11,4);("Hendy","Wilson",11,8);("Thomas","Claire",5,11);("Thomas","Hendy",2,11);("Wilson","Thomas",11,1);("Connor","Wilson",11,8);("Connor","Thomas",11,1);("Hendy","Connor",11,7);("Wilson","Claire",11,5);("Claire","Connor",8,11);("Jonathon","Scott",5,11);("Jonathon","Micheal",1,11);("Micheal","Scott",8,11);("Ben","Micheal",12,10);("Micheal","Brooke",11,2);("Ben","Jonathon",12,10);("Brooke","Ben",2,11);("Scott","Brooke",5,11);("Scott","Ben",7,11);("Brooke","Jonathon",11,2)]

(* Expected win rate of ranking `ra` against `rb` *)
let winRate ra rb = 1. /. (1. +. 10. ** ((rb -. ra) /. 400.))

(* The points someone with rating `ra` gains and his opponent loses *)
let updateElo ra rate = 20. *. (1. -. rate)

let rec getRating a = function
  |  []   -> 1200.
  | x::xs -> if a = fst x then snd x
             else getRating a xs

let rec updateRating a d = function
  |  []   -> [(a, 1200. +. d)]
  | x::xs -> if a = fst x then (a, snd x +. d) :: xs
             else x :: updateRating a d xs

let maxDifference xs =
  let minimum = List.fold_left min infinity xs in
  let maximum = List.fold_left max neg_infinity xs in
  truncate maximum - truncate minimum

let answer =
  let rec f ratings (a, b, pa, pb) =
    let ra = getRating a ratings in
    let rb = getRating b ratings in
    if pa > pb then
      let delta = winRate ra rb |> updateElo ra in
      updateRating a delta ratings |> updateRating b (-.delta)
    else (* assumes no ties *)
      let delta = winRate rb ra |> updateElo rb in
      updateRating b delta ratings |> updateRating a (-.delta)
  in List.fold_left f [] input |> List.map snd |> maxDifference
