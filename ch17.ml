#use "topfind";;
#require "calendar";;

let parseDate s =
    let year = String.sub s 0 4 |> int_of_string in
    let month = String.sub s 5 2 |> int_of_string in
    let day = String.sub s 8 2 |> int_of_string in
    CalendarLib.Date.make year month day

let dateString d =
    let open CalendarLib.Date in
    let int2str = Printf.sprintf "%02d" in
    int2str (year d) ^ int2str (month d |> int_of_month) ^ int2str (day_of_month d)

let outputString team since until =
    team ^ " " ^ dateString since ^ " " ^ dateString until

let answer = 
    let shame = ref [] in
    let candidates = ref [] in
    let processLine date team score =
        if score = "0" && not (List.mem_assoc team !shame) then 
            shame := (team, parseDate date) :: !shame
        else if score <> "0" && List.mem_assoc team !shame then
            let date0 = List.assoc team !shame in
            let date1 = parseDate date in
            let period = CalendarLib.Date.sub date1 date0 in
            shame := List.remove_assoc team !shame;
            candidates := (period, outputString team date0 date1) :: !candidates in
    let f = open_in "football.txt" in
    let rec loop () =
        try let line = input_line f in
            match String.split_on_char ',' line with
            | date :: team1 :: team2 :: score1 :: score2 :: _ ->
                    processLine date team1 score1;
                    processLine date team2 score2;
                    loop ()
            | _ -> failwith "bad data"
        with End_of_file -> close_in f
    in loop ();
    List.sort (fun x y -> compare x y |> Int.neg) !candidates |> List.hd |> snd
