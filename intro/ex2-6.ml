(* 2. *)
let jpy_to_usd jpy = (float_of_int jpy) /. 111.12

(* 4. *)
open Char
let capitalize_char ch =
  match ch with
    'a'..'z' -> uppercase_ascii ch
  | _ -> ch

open String
let capitalize str = map capitalize_char str
