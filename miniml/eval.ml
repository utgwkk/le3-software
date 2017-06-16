open Syntax 

type exval = 
    IntV of int
  | BoolV of bool
  | ListV of exval list
  | DProcV of id * exp
  | ProcV of id * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ListV l -> "[" ^ (String.concat "; " (List.map string_of_exval l)) ^ "]"
  | ProcV (name, _, _) -> "<function " ^ name ^ ">"
  | DProcV (name, _) -> "<dynamic function " ^ name ^ ">"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err ("Both arguments must be integer: -")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Cons, ListV l1, ListV l2 -> ListV ((ListV l1) :: l2)
  | Cons, x, ListV [] -> ListV [x]
  | Cons, IntV x, ListV ((IntV _) :: _ as xs) -> ListV ((IntV x) :: xs)
  | Cons, _, ListV ((IntV _) :: _) -> err ("The left argument must be integer: ::")
  | Cons, BoolV x, ListV ((BoolV _) :: _ as xs) -> ListV ((BoolV x) :: xs)
  | Cons, _, ListV ((BoolV _) :: _) -> err ("The left argument must be boolean: ::")
  | Cons, (DProcV _ as p), ListV ((DProcV _) :: _ as xs) -> ListV (p :: xs)
  | Cons, _, ListV ((DProcV _) :: _) -> err ("The left argument must be dynamic function: ::")
  | Cons, (ProcV _ as p), ListV ((ProcV _) :: _ as xs) -> ListV (p :: xs)
  | Cons, _, ListV ((ProcV _) :: _) -> err ("The left argument must be non-dynamic function: ::")
  | Cons, _, _ -> err ("The right argument must be a list: ::")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
  | Eq, BoolV i1, BoolV i2 -> BoolV (i1 = i2)
  | Eq, _, _ -> err ("Both arguments must be the same type: =")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")

let rec apply_prim_lazy op env exp1 exp2 =
  let arg1 = eval_exp env exp1 in
    match op, arg1 with
      And, BoolV false -> BoolV false
    | Or, BoolV true -> BoolV true
    | _, _ -> eval_exp env exp2

and eval_exp env = function
    Var x -> 
      (try Environment.lookup x !env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | LLit l -> ListV (List.map (eval_exp env) l)
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | LazyBinOp (op, exp1, exp2) ->
      apply_prim_lazy op env exp1 exp2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
      let value = eval_exp env exp1 in
      eval_exp (ref (Environment.extend id value !env)) exp2
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | DFunExp (id, exp) -> DProcV (id, exp)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
          ProcV (id, body, env') ->
            let newenv = ref (Environment.extend id arg !env') in
            eval_exp newenv body
        | DProcV (id, body) ->
            let newenv = ref (Environment.extend id arg !env) in
            eval_exp newenv body
        | _ -> err ("Non-function value is applied")
      )
  | LetRecExp (id, para, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
      let newenv =
        Environment.extend id (ProcV (para, exp1, dummyenv)) !env in
      dummyenv := newenv;
      eval_exp dummyenv exp2
  | MatchExp (target, head, tail, exp1, exp2) -> (
      let target_value = eval_exp env target
      in match target_value with
        ListV [] -> eval_exp env exp1
      | ListV (x :: xs) -> let newenv =
          ref (Environment.extend head x (Environment.extend tail (ListV xs) !env))
        in eval_exp newenv exp2
      | _ -> err ("Pattern match target must be list")
  )

let eval_decl env = function
    Exp e -> let v = eval_exp (ref env) e in ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp (ref env) e in (id, Environment.extend id v env, v)
  | RecDecl (id, _, e) ->
      let dummyenv = ref Environment.empty in
      let newenv =
        Environment.extend id (eval_exp dummyenv e) env in
      dummyenv := newenv;
      let v = eval_exp dummyenv e in (id, Environment.extend id v env, v)
