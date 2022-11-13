(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [([55;285;27;323;22;400;20;49;40;336;50;98;36;12;96;294],[0;2;3;0;0;7;0;0;17;19;20;0;0;42;48;0]);([70;52;196;17;53;234;35;114;150;29;276;41;31;264;50;78],[3;3;0;6;6;7;0;0;0;0;26;0;38;44;0;0]);([35;150;41;77;297;17;133;38;52;352;26;360;66;42;264;18],[0;0;0;0;7;8;0;11;0;0;0;0;30;0;33;0]);([116;18;56;24;10;90;20;60;33;16;100;19;23;25;17;70],[1;0;2;3;4;0;0;0;0;0;14;0;0;0;0;0]);([60;23;13;6;12;112;17;48;22;18;105;30;26;7;19;9],[0;0;2;3;3;3;0;6;0;8;12;14;0;16;0;21]);([25;13;72;30;69;29;100;24;11;9;26;22;15;36;10;54],[1;2;3;3;3;0;0;0;0;9;10;12;18;20;0;0]);([132;120;23;27;180;27;130;29;198;28;31;126;168;92;23;26],[0;0;6;0;9;9;10;0;0;0;0;0;0;0;22;0]);([10;12;25;99;45;20;16;28;64;20;15;14;8;36;11;16],[0;2;0;3;5;5;5;5;0;8;9;0;11;11;14;18]);([12;13;26;17;40;42;18;30;35;13;45;70;30;17;14;15],[0;2;3;0;4;5;6;0;7;7;0;10;10;13;0;15]);([27;198;126;37;102;29;132;25;180;144;28;23;210;37;160;41],[0;5;6;6;6;0;9;11;16;0;18;0;0;0;0;0]);([144;30;24;180;21;176;26;140;28;27;160;98;33;182;27;200],[0;7;8;8;10;0;0;12;0;14;14;0;18;0;0;0])]

let divisors n =
  let rec f k =
    if k * k > n then
      []
    else
      if n mod k = 0 then
        (k, n / k) :: f (k + 1)
      else
        f (k + 1)
  in f 1

let pairs xs =
  let rec f = function
    | []    -> []
    | y::ys -> let ds = divisors y in
               let pred (x,y) = List.mem (x + y) xs in
               List.filter pred ds @ f ys
  in f xs

let score ps =
  let f n (x, y) = n + y - x in
  List.fold_left f 0 ps

let check ps seq =
  let (s1, s2) = List.to_seq ps |> Seq.unzip in
  let xs = Seq.append s1 s2 |> List.of_seq |> List.sort compare in
  let f x y = x = y || y = 0 in
  List.for_all2 f xs seq

let rec select a = function
  | []    -> None
  | x::xs -> if x = a then Some xs
             else match select a xs with
                      | None     -> None
                      | Some xs' -> Some (x :: xs')

let rec solution (grid,seq) ps =
  if grid = [] then
    if check ps seq then Some (score ps) else None
  else
    let rec f (x,y) =
      match select (x + y) grid |> Option.map (select (x * y)) |> Option.join with
      | None       -> None
      | Some grid' -> solution (grid',seq) ((x,y) :: ps)
    in List.find_map f (pairs grid)

let answer =
  let f n t = n + Option.get (solution t []) in
  List.fold_left f 0 input
