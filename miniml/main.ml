open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try (
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let ty = ty_decl tyenv decl in
      let (id, newenv, v) = eval_decl env decl in
        Printf.printf "val %s : " id;
        pp_ty ty;
        print_string " = ";
        pp_val v;
        print_newline();
        read_eval_print newenv tyenv
  ) with
  Error e -> (
    Printf.printf "Runtime Error: %s " e;
    print_newline();
    read_eval_print env tyenv
  )
  | Failure e -> (
    Printf.printf "Parse Error: %s " e;
    print_newline();
    read_eval_print env tyenv
  )
  | _ -> (
    Printf.printf "Syntax Error";
    print_newline();
    read_eval_print env tyenv
  )

let initial_env =
  let rec extend env = function
      [] -> env
    | (name, value) :: rest -> extend (Environment.extend name value env) rest
  in extend Environment.empty [
      ("i", (IntV 1));
      ("v", (IntV 5));
      ("x", (IntV 10));
      ("ii", (IntV 2));
      ("iii", (IntV 3));
      ("iv", (IntV 4));
      ]

let initial_tyenv =
  let rec extend env = function
      [] -> env
    | (name, t) :: rest -> extend (Environment.extend name t env) rest
  in extend Environment.empty [
      ("i", TyInt);
      ("v", TyInt);
      ("x", TyInt);
      ("ii", TyInt);
      ("iii", TyInt);
      ("iv", TyInt);
      ]

let _ = read_eval_print initial_env initial_tyenv
