type nat = Zero | OneMoreThan of nat

let rec int_of_nat = function
    Zero -> 0
  | OneMoreThan x -> 1 + int_of_nat x

(* a * b = a + a + ... + a (b times) *)
let rec mul m n =
  let rec add m n =
    match m with
      Zero -> n
    | OneMoreThan m' -> OneMoreThan (add m' n)
  in
  match m with
    Zero -> Zero
  | OneMoreThan m' ->
      match n with
        Zero -> Zero
      | OneMoreThan n' -> add m (mul m n')

(* a - b = (a - 1) - (b - 1) *)
let rec monus m n =
  match m with
    Zero -> Zero
  | OneMoreThan m' ->
      match n with
        Zero -> m
      | OneMoreThan n' -> monus m' n'

(* Constants for testing *)
let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan (one)
let three = OneMoreThan (two)
let four = OneMoreThan (three)
let five = OneMoreThan (four)
