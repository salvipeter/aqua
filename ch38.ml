let input =
  let f = open_in "comfort.txt" in
  let rec loop () =
    let line = try Some (input_line f |> String.trim)
               with End_of_file -> close_in f; None
    in match line with
       | Some line -> let nums = String.split_on_char ' ' line in
                      List.map int_of_string nums :: loop ()
       | None      -> []
  in loop ()

(* A streak of length `l`, starting at index `k` *)
let rec streak xs k l =
  match (k, l) with
  | _, 0 -> []
  | 0, l -> List.hd xs :: streak (List.tl xs) 0 (l - 1)
  | k, l -> streak (List.tl xs) (k - 1) l

let comfy xs = List.fold_left (+) 0 xs mod List.length xs = 0

(* Is there a comfortable streak of length `l` including the `k`-th element? *)
let comfortable xs k l =
  let n = List.length xs in
  let rec f i = i <= k && i <= n - l && (comfy (streak xs i l) || f (i + 1))
  in f (max (k - l + 1) 0)

(* Note: not the maximal streak, but the longest cozy streak from 1 up *)
let cozyness xs k =
  let n = List.length xs in
  let rec f l streak =
    if l <= n && comfortable xs k l then
      f (l + 1) (streak + 1)
    else
      streak
  in f 1 0

let answer =
  let f n xs = let ys = List.init (List.length xs) (cozyness xs) in
               List.fold_left (+) n ys
  in List.fold_left f 0 input
