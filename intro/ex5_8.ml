(* map function with tail call. *)
let map2 f l =
  let rec map2' f mapped = function
      [] -> mapped
    | x :: xs -> map2' (f) (mapped @ [f x]) xs
  in map2' f [] l
