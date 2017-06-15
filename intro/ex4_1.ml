(* generate list of int s.t. [m; m + 1; ...; n] *)
let rec int_seq m n =
  if m > n then []
  else m :: (int_seq (m + 1) n)

let segment f a b n i =
  let delta = (b -. a) /. (float_of_int n) in
  (f (a +. (float_of_int (i - 1)) *. delta) +. f (a +. (float_of_int i) *. delta)) *. delta /. 2.

let sum xs = List.fold_left (+.) 0.0 xs

let integral f a b =
  sum @@ List.map (segment f a b 100) (int_seq 1 100)

let pi = 3.14159265359

let sin = Pervasives.sin
