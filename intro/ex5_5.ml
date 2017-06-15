(* true && (b0 && (b1 && ... ) ) *)
let forall p xs = List.fold_right (&&) (List.map p xs) true

(* false || (b0 || (b1 || ... ) ) *)
let exists p xs = List.fold_right (||) (List.map p xs) false
