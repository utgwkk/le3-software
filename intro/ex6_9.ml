type 'a seq = Cons of 'a * (unit -> 'a seq)

let head (Cons (x, _)) = x
let tail (Cons (_, f)) = f ()
let rec take n s = if n = 0 then [] else head s :: take (n - 1) (tail s)
let rec from n = Cons (n, fun () -> from (n + 1))

let rec sift n (Cons (x, f)) =
  let first = if x mod n = 0 then head (f ()) else x
  and second = if x mod n = 0 then tail (f ()) else f ()
  in Cons (first, fun () -> sift n second)

(* 
# nthseq (2922 + 3000) primes;;
- : int = 58481
*)

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f())))
let primes = sieve (from 2)
let rec nthseq n (Cons (x, f)) =
  if n = 1 then x else nthseq (n - 1) (f())
