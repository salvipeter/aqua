#use "ch34-input.ml";;

(*
let input = [|[|"station";"r1";"r2";"r3"|];
              [|"a";"00:01";"";"00:02"|];
              [|"b";"00:16";"";"00:17"|];
              [|"c";"";"00:21";""|];
              [|"d";"00:46";"00:51";"00:47"|]|]
*)

let to_mins str =
    let digit c = int_of_char c - int_of_char '0' in
    let h = digit (String.get str 0) * 10 + digit (String.get str 1) in 
    let m = digit (String.get str 3) * 10 + digit (String.get str 4) in 
    h * 60 + m

(* name, last_station, [(station, time); ...] *)
type train = Train of string * string * (string * int) list

(* name, Some (train, time_in), [(waiting_train_name, last_station); ...] *)
type station = Station of string * (string * int) option * (string * string) list

(* Inserts train t into the waiting queue *)
let rec insert_queue (Train (name, last, _) as t) = function
    | []         -> [(name, last)]
    | (n,l)::nls ->
            if last < l || last = l && name < n then
                (name, last) :: (n, l) :: nls
            else
                (n, l) :: insert_queue t nls

(* Updates the station list s.t. train t enters the queue of station named sname *)
let rec enter_queue t sname = function
    | [] -> failwith "no such station"
    | Station (name, train_in, queue) as s :: ss ->
            if name <> sname then
                s :: enter_queue t sname ss
            else
                let queue' = insert_queue t queue in
                Station (name, train_in, queue') :: ss
   
(* Handles the arrival of trains at the specified time *)
let arrive time ts ss =
    let rec f ss acc = function
        | [] -> acc, ss
        | Train (name, last, nexts) as t :: ts ->
                match nexts with
                | [] -> f ss (t :: acc) ts
                | (s, time') :: ns ->
                        if time' <> time then
                            f ss (t :: acc) ts
                        else begin
                            (* print_string "Train "; print_string name;
                            print_string " enters the queue at "; print_endline s; *)
                            let ss' = enter_queue t s ss in
                            f ss' (Train (name, s, ns) :: acc) ts
                        end
    in f ss [] ts

(* Adds 5 minutes to all times in the train's schedule *)
let rec wait5 tname = function
    | [] -> failwith "no such train"
    | Train (name, last, stops) as t :: ts ->
            if name <> tname then
                t :: wait5 tname ts
            else
                let wait (stop, time) = stop, time + 5 in
                let stops' = List.map wait stops in
                Train (name, last, stops') :: ts

(* Next trains enter all empty stations *)
let enter_station ts ss =
    let rec f ts acc = function
        | [] ->
                ts, acc
        | Station (_, Some _, _) as s :: ss ->
                f ts (s :: acc) ss
        | Station (_, None, []) as s :: ss ->
                f ts (s :: acc) ss
        | Station (name, None, (t, _)::queue) :: ss ->
                (* print_string "Train "; print_string t;
                print_string " enters station "; print_endline name; *)
                let s' = Station (name, Some (t, 1), queue) in
                let ts' = wait5 t ts in
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
    | Station (name, Some (t, n), queue) as s :: ss ->
            if n < 5 then
                let s' = Station (name, Some (t, n + 1), queue) in
                f finished (s' :: acc) ss
            else begin
                (* print_string "Train "; print_string t;
                print_string " departs station "; print_endline name; *)
                let s' = Station (name, None, queue) in
                if completed_journey t ts then
                    begin
                        (* print_string "Train "; print_string t;
                        print_endline " completes its journey"; *)
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

let answer =
    let n = Array.length input - 1 in
    let m = Array.length input.(0) - 1 in
    let rec stops t s =
        if s > n then []
        else match input.(s).(t) with
        | ""   -> stops t (s + 1)
        | time -> (input.(s).(0), to_mins time) :: stops t (s + 1) in
    let train t = Train (input.(0).(t), "", stops t 1) in
    let ts = Seq.ints 1 |> Seq.take m |> Seq.map train |> List.of_seq in
    let station s = Station (input.(s).(0), None, []) in
    let ss = Seq.ints 1 |> Seq.take n |> Seq.map station |> List.of_seq in
    let max_time = ref 0 in
    let rec process ts ss time =
        (* print_string "["; print_int time; print_endline "]"; *)
        let ts, ss = arrive time ts ss in
        let ss, finishing = depart ts ss in
        let finish_times = List.map (fun t -> time - start_time input t) finishing in
        max_time := List.fold_left max !max_time finish_times;
        let ts, ss = enter_station ts ss in
        if not (finished ts ss) then
            process ts ss (time + 1)
    in process ts ss 0;
    !max_time
