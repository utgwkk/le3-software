(* 2. *)
let jpy_to_usd jpy = (float_of_int jpy) /. 111.12

(* 4. *)
let capitalize str =
  let capitalize_char ch =
    match ch with
      'a'..'z' -> Char.uppercase_ascii ch
    | _ -> ch
  in String.map capitalize_char str
