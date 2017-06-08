let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x) else x

let fib n =
  let (fibn, _) =
    let next (a, b) = (b, a + b)
    in repeat next n (1, 0)
  in fibn;;
