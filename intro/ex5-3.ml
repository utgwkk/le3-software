(* 1. *)
let rec downto0 = function
    0 -> [0]
  | n -> n :: downto0 (n - 1)

(* 3. *)
let rec concat l = match l with
    [] -> []
  | x::xs -> x @ concat xs

(* 4. *)
let rec zip la lb = match la with
    [] -> []
  | x::xs -> match lb with
        [] -> []
      | y::ys -> (x, y) :: zip xs ys

(* 5. *)
let rec filter f xs = match xs with
    [] -> []
  | x::xs -> if f x then x::(filter f xs) else filter f xs
