type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec reflect tr =
  match tr with
    Lf -> Lf
  | Br (value, left, right) -> Br (value, reflect right, reflect left)
