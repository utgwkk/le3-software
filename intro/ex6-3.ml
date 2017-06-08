type nat = Zero | OneMoreThan of nat

(* Optional *)
let rec monus m n =
  match m with
    Zero -> if n = Zero then Some Zero else None
  | OneMoreThan m' ->
      match n with
        Zero -> Some m
      | OneMoreThan n' -> monus' m' n'

(* Constants for testing *)
let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan (one)
let three = OneMoreThan (two)
let four = OneMoreThan (three)
let five = OneMoreThan (four)
