#use "ch10-input.ml";;

let bellmanFord edges a b =
  let rec f dists =
    let changed = ref false in
    let rec g dists (u, v, w) =
      match (List.assoc_opt u dists, List.assoc_opt v dists) with
      | (None,    _)       -> dists
      | (Some du, None)    -> ( changed := true; (v, du + w) :: dists )
      | (Some du, Some dv) -> if du + w >= dv then dists
                              else ( changed := true
                                   ; (v, du + w) :: List.remove_assoc v dists
                                   )
    in let dists' = List.fold_left g dists edges in
       if !changed then f dists' else dists
  in f [(a, 0)] |> List.assoc b

let answer = bellmanFord input "TUPAC" "DIDDY"
