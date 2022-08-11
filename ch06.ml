let input = 123

let rec ones n =
  let k = n mod 10 in
  let base = if k == 1 then 1 else 0 in
  if n > 9 then base + ones (n / 10) else base

let answer =
  let sum = ref 0 in
  for a = 0 to input do
    for b = 0 to input - a do
      let c = input - a - b in
      sum := !sum + ones a + ones b + ones c
    done
  done;
  !sum
