(* 1. *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(* 2. *)
let rec comb n m =
  if (m = 0 || m = n) then 1
  else (comb (n - 1) m) + (comb (n - 1) (m - 1))

(* 3. *)
let rec fib_iter n m a b =
  if m = n then b
  else fib_iter n (m + 1) b (a + b)

(* 4. *)
let max_ascii str =
  let get_max a b = if (Char.code a > Char.code b) then a else b in
  let rec max_ascii_inner str idx ch =
    if idx = String.length str then ch else max_ascii_inner str (idx + 1) (get_max str.[idx] ch)
  in
  max_ascii_inner str 0 (Char.chr 0)
