(* integrate f from a to b. *)
let integral f a b =
  (* generate list of int s.t. [m; m + 1; ...; n] *)
  let rec int_seq m n =
    if m > n then []
    else m :: (int_seq (m + 1) n)
  in
  let sum xs = List.fold_left (+.) 0.0 xs
  (* Trapezoidal rule. *)
  and segment f a b n i =
        let delta = (b -. a) /. (float_of_int n) in
        (f (a +. (float_of_int (i - 1)) *. delta) +. f (a +. (float_of_int i) *. delta)) *. delta /. 2.
  in
    sum @@ List.map (segment f a b 100) (int_seq 1 100)

let integrate_sin_from_0_to_pi =
  let pi = 3.14159265359
  and sin = Pervasives.sin
  in
    integral sin 0. pi
