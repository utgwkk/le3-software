open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try (
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      let (id, newenv, v) = eval_decl env decl in
        Printf.printf "val %s = " id;
        pp_val v;
        print_newline();
        read_eval_print newenv
  ) with
  Error e -> (
    Printf.printf "Runtime Error: %s " e;
    print_newline();
    read_eval_print env
  )
  | Failure e -> (
    Printf.printf "Parse Error: %s " e;
    print_newline();
    read_eval_print env
  )
  | _ -> (
    Printf.printf "Syntax Error";
    print_newline();
    read_eval_print env
  )

let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))

let _ = read_eval_print initial_env
