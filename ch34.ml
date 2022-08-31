let input =
    let process line = String.split_on_char ',' line |> Array.of_list in
    let f = open_in "trains.csv" in
    let rec loop xs =
        try loop (process (input_line f) :: xs)
        with End_of_file -> close_in f; xs
    in loop [] |> List.rev |> Array.of_list

(*
let input = [|[|"station";"r1";"r2";"r3"|];
              [|"a";"00:01";"";"00:02"|];
              [|"b";"00:16";"";"00:17"|];
              [|"c";"";"00:21";""|];
              [|"d";"00:46";"00:51";"00:47"|]|]
*)

(*
  The challenge text is VERY MISLEADING.
  While the signs are correct, the text says:

      "Any further ties are broken by route number - e.g. for two trains attempting
       to originate at station a at the same time on r1 and r2, r1 would enter first."

  But this tie-breaking is only done _among the newly queued trains_, any train that
  is already queued and of the same last station will come first.
 *)

let to_int seq =
    let f n c = n * 10 + int_of_char c - int_of_char '0' in
    Seq.fold_left f 0 seq

let to_mins str =
    let h = String.sub str 0 2 |> String.to_seq |> to_int in
    let m = String.sub str 3 2 |> String.to_seq |> to_int in
    h * 60 + m

(* for debugging *)
let to_time mins =
    let h = (mins / 60) mod 24 in
    let m = mins mod 60 in
    let d n = char_of_int (int_of_char '0' + n) |> String.make 1 in
    d (h / 10) ^ d (h mod 10) ^ ":" ^ d (m / 10) ^ d (m mod 10)

(* name, last_station, [(station, time); ...] *)
type train = Train of string * string * (string * int) list

(* name, Some (train, time_in), [(waiting_train_name, last_station, original_time); ...] *)
type station = Station of string * (string * int) option * (string * string * int) list

(* Inserts train t into the waiting queue *)
let rec insert_queue t qs =
    match t with
    | Train (_, _, []) -> failwith "invalid train"
    | Train (name, last, (_, time) :: stops) ->
            match qs with
            | [] -> [(name, last, time)]
            | (_, l, _) as q :: qs ->
                    if last < l then
                        (name, last, time) :: q :: qs
                    else
                        q :: insert_queue t qs

(* Updates the station list s.t. train t enters the queue of station named sname *)
let rec enter_queue t sname = function
    | [] -> failwith "no such station"
    | Station (name, train_in, queue) as s :: ss ->
            if name <> sname then
                s :: enter_queue t sname ss
            else
                let queue' = insert_queue t queue in
                Station (name, train_in, queue') :: ss
   
(* Is the train at or outside a station? *)
let rec occupied tname = function
    | [] -> false
    | Station (_, train_in, queue) :: ss ->
            occupied tname ss ||
            List.exists (fun (name, _, _) -> name = tname) queue ||
            match train_in with
            | None           -> false
            | Some (name, _) -> name = tname

(* Handles the arrival of trains at the specified time *)
let arrive time ts ss =
    let rec f ss acc = function
        | [] -> List.rev acc, ss (* trains should remain in route order *)
        | Train (name, last, nexts) as t :: ts ->
                match nexts with
                | [] -> f ss (t :: acc) ts
                | (s, time') :: ns ->
                        if time' <> time || occupied name ss then
                            f ss (t :: acc) ts
                        else begin
                            print_endline ("Train " ^ name ^ " enters the queue at " ^ s);
                            let ss' = enter_queue t s ss in
                            f ss' (Train (name, s, ns) :: acc) ts
                        end
    in f ss [] ts

(* Adds n minutes to all times in the train's schedule *)
let rec wait n tname = function
    | [] -> failwith "no such train"
    | Train (name, last, stops) as t :: ts ->
            if name <> tname then
                t :: wait n tname ts
            else
                let add (stop, time) = stop, time + n in
                let stops' = List.map add stops in
                Train (name, last, stops') :: ts

(* Next trains enter all empty stations *)
let enter_station time ts ss =
    let rec f ts acc = function
        | [] ->
                ts, acc
        | Station (_, Some _, _) as s :: ss ->
                f ts (s :: acc) ss
        | Station (_, None, []) as s :: ss ->
                f ts (s :: acc) ss
        | Station (name, None, (t, _, time')::queue) :: ss ->
                print_endline ("Train " ^ t ^ " enters station " ^ name);
                let s' = Station (name, Some (t, 1), queue) in
                let ts' = wait (time - time' + 5) t ts in
                f ts' (s' :: acc) ss
    in f ts [] ss

(* Checks if the train with the given name has completed its journey *)
let rec completed_journey tname = function
    | [] -> failwith "no such train"
    | Train (name, _, queue) :: ts ->
            if name = tname then
                queue = []
            else
                completed_journey tname ts

(* Trains depart their stations after 5 minutes of waiting;
   also returns a list of train names that completed their tasks *)
let depart ts ss =
    let rec f finished acc = function
    | [] -> acc, finished
    | Station (_, None, _) as s :: ss ->
            f finished (s :: acc) ss
    | Station (name, Some (t, n), queue) :: ss ->
            if n < 5 then
                let s' = Station (name, Some (t, n + 1), queue) in
                f finished (s' :: acc) ss
            else begin
                print_endline ("Train " ^ t ^ " departs station " ^ name);
                let s' = Station (name, None, queue) in
                if completed_journey t ts then
                    begin
                        print_endline ("Train " ^ t ^ " completes its journey");
                        f (t :: finished) (s' :: acc) ss
                    end
                else
                    f finished (s' :: acc) ss
            end
    in f [] [] ss

(* Nothing else to do? *)
let finished ts ss =
    let empty = function
        | Station (_, None, []) -> true
        | _                     -> false
    and completed = function
        | Train (_, _, []) -> true
        | _                -> false
    in List.for_all empty ss && List.for_all completed ts

(* Time of first task for the train with name tname *)
let start_time table tname =
    let rec loop j =
        if input.(0).(j) <> tname then
            loop (j + 1)
        else
            let rec find i =
                match input.(i).(j) with
                | ""  -> find (i + 1)
                | str -> to_mins str
            in find 1
    in loop 1

let init_trains table =
    let m = Array.length input.(0) - 1 in
    let n = Array.length input - 1 in
    let rec stops t s =
        if s > n then []
        else match input.(s).(t) with
        | ""   -> stops t (s + 1)
        | time -> (input.(s).(0), to_mins time) :: stops t (s + 1) in
    let train t = Train (input.(0).(t), "", stops t 1) in
    Seq.ints 1 |> Seq.take m |> Seq.map train |> List.of_seq

let init_stations table =
    let n = Array.length input - 1 in
    let station s = Station (input.(s).(0), None, []) in
    Seq.ints 1 |> Seq.take n |> Seq.map station |> List.of_seq

let answer =
    let ts = init_trains input in
    let ss = init_stations input in
    let max_time = ref 0 in
    let rec process ts ss time =
        print_endline ("[" ^ to_time time ^ "]");
        let ts, ss = arrive time ts ss in
        let ss, finishing = depart ts ss in
        let compute_time t =
            let d = time - start_time input t in
            print_string ("Completion time for " ^ t ^ ": ");
            print_int d; print_newline ();
            d in
        let finish_times = List.map compute_time finishing in
        max_time := List.fold_left max !max_time finish_times;
        let ts, ss = enter_station time ts ss in
        if not (finished ts ss) then
            process ts ss (time + 1)
    in process ts ss 0;
    !max_time
