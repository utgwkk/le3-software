(* int -> int -> int -> int*)
let add3 x y z = x + y + z

(* (int -> int) -> int -> int *)
let apply_next f x = f (x + 1) + 1

(* (int -> int -> int) -> int *)
let force_1_2_then_succ f = (f 1 2) + 1
