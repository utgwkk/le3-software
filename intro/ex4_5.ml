(* apply function f with n times. *)
let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x) else x

(* apply function s.t. (a, b) -> (b, a + b) with n times
 * and returns the first element of tuple.
 * *)
let fib n =
  let (fibn, _) =
    let next (a, b) = (b, a + b)
    in repeat next n (1, 0)
  in fibn;;
