(* 1. *)
type bt =
    Lf
  | Br of int * bt * bt

(* 2. *)
let rec sumtree tr =
  match tr with
    Lf -> 0
  | Br (value, left, right) -> value + (sumtree left) + (sumtree right)

(* 3. *)
let rec mapTree f tr =
  match tr with
    Lf -> Lf
  | Br (value, left, right) -> Br (f value, mapTree f left, mapTree f right)
