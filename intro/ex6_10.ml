type ('a, 'b) sum = Left of 'a | Right of 'b

(*
   val union : 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum = <fun>
   val apply_either : ('a -> 'b) * ('c -> 'b) -> ('a, 'c) sum -> 'b = <fun>
   val either : ('a -> 'b, 'a -> 'c) sum -> 'a -> ('b, 'c) sum = <fun>
 *)

(* 1. *)
let union (a, s) =
  match s with
    Left b -> Left (a, b)
  | Right c -> Right (a, c)

(* 3. *)
let apply_either (f, g) s =
  match s with
    Left a -> f a
  | Right b -> g b

(* 5. *)
let either s =
  match s with
    Left f -> (fun x -> Left (f x))
  | Right g -> (fun x -> Right (g x))
