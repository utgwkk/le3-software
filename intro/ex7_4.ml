(* Calculates factorial (n!) with reference. *)
let fact_imp n =
  let i = ref n and res = ref 1 in
    while (!i > 0) do
      res := !res * !i;
      i := !i - 1;
    done;
  !res
