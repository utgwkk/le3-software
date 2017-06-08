(* 1. *)
let rec pow x n =
  if n <= 0 then 1
  else x * (pow x (n - 1))

(* 2. *)
let rec pow' x n =
  if n <= 0 then 1
  else
    if n mod 2 == 0 then
      (pow' x (n / 2)) * (pow' x (n / 2))
    else x * (pow' x (n - 1))
