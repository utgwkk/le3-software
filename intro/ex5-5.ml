let forall p xs = List.fold_right (&&) (List.map p xs) true
let exists p xs = List.fold_right (||) (List.map p xs) false
