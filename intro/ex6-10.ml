type ('a, 'b) sum = Left of 'a | Right of 'b

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
