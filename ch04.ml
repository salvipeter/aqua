let input = 987820

let factors n =
  let rec f n k =
    if n == 1 then []
    else if n mod k == 0 then k :: f (n / k) k
    else f n (k + 1)
  in f n 2

let rec unique = function
  | []       -> []
  | [x]      -> [x]
  | x::y::xs -> if x == y then unique (y::xs)
                else x :: unique (y::xs)

let coprime primes n = not (List.exists (fun p -> n mod p == 0) primes)

let answer =
  let primes = factors input |> unique in
  let sum = ref 0 in
  for n = 1 to input - 1 do
    if coprime primes n then
      sum := !sum + n
  done;
  !sum
