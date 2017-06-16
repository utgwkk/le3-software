(* map function with tail call. *)
let map2 f l =
  let rec map2' f mapped = function
      [] -> []
    | [x] -> List.rev_append mapped [f x]
    | x :: xs -> map2' f ((f x) :: mapped) xs
  in map2' f [] l
