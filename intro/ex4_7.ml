(*
# s;;
- : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c = <fun>
# k;; 
- : 'a -> 'b -> 'a = <fun>

s k k 1
= k 1 (k 1)
= 1

`k (s k k) x y` always returns y.
*)
let s x y z = x z (y z)
let k x y = x
let i x = (s k k) x
