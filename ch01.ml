let input = "do you think that maybe like, 1 in 10 people could be actually robots?"

let isHex c = 'a' <= c && c <= 'f' || '0' <= c && c <= '9'

let takeChar s i =
  let c = String.get s i in
  if isHex c then c else '0'

let answer =
  let n = String.length input in
  let k = (n + 2) / 3 in
  let indices = [0; 1; k; k + 1; 2 * k; 2 * k + 1] in
  List.map (takeChar input) indices |> List.to_seq |> String.of_seq
