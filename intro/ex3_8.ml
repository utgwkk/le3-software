(*
 * To calculate x^n, call this function like:
   # powi x n 1

   for example:
   # powi 2 10 1
   - : int = 1024
 * *)

let rec powi x n result =
  if n = 0 then result
  else powi x (n - 1) (x * result)
