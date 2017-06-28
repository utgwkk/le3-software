open Syntax

exception Error of string

let err s = raise (Error s)

(* Type environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let rec subst_type subs t =
  let rec substitute (tyvar, tyrepl) = function
    TyInt -> TyInt
  | TyBool -> TyBool
  | TyList a -> TyList (substitute (tyvar, tyrepl) a)
  | TyVar a -> if a = tyvar then tyrepl else TyVar a
  | TyFun (a, b) -> TyFun ((substitute (tyvar, tyrepl) a), (substitute (tyvar, tyrepl) b))
  in
  match subs with
    [] -> t
  | h::ta -> subst_type ta (substitute h t)

let eqs_of_subst s = List.map (fun (tyv, t) -> (TyVar tyv, t)) s

let subst_eqs s eqs = List.map (fun (t1, t2) -> (subst_type s t1, subst_type s t2)) eqs

let rec unify =
  let rec ftv tyvar = function
    TyFun (a, b) -> (ftv tyvar a) || (ftv tyvar b)
  | TyVar a -> tyvar = a
  | TyList a -> ftv tyvar a
  | _ -> false
  in function
      [] -> [] | (t1, t2)::tl ->
        (* pp_ty t1; *)
        (* print_string ", "; *)
        (* pp_ty t2; *)
        (* print_newline(); *)
        if t1 = t2 then unify tl else (
        match (t1, t2) with
          (TyFun (t11, t12), TyFun (t21, t22)) -> unify ((t11, t21)::(t12, t22)::tl)
        | (TyList t1, TyList t2) -> unify ((t1, t2)::tl)
        | (TyVar t1, TyVar t2) -> (t2, TyVar t1) :: (unify @@ subst_eqs [(t2, TyVar t1)] tl)
        | (TyVar tv, t) ->
            if ftv tv t then err ("type variable " ^ (string_of_int tv) ^ " appears.")
            else (tv, t) :: (unify @@ subst_eqs [(tv, t)] tl)
        | (t, TyVar tv) ->
            unify @@ (TyVar tv, t)::tl
        | _ -> err "Type Unification Error")

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Minus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Cons ->
      let a = TyVar (fresh_tyvar ()) in ([(ty1, a); (ty2, TyList a)], TyList a)
  | _ -> err "ty_prim: Not implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | LLit l -> (
      match l with
      [] -> ([], TyList (TyVar (fresh_tyvar ())))
    | h::t -> ty_exp tyenv h
  )
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp_cond, exp_t, exp_f) ->
      let (s1, ty_cond) = ty_exp tyenv exp_cond in
      let (s2, ty_t) = ty_exp tyenv exp_t in
      let (s3, ty_f) = ty_exp tyenv exp_f in
      let eqs = (ty_cond, TyBool) :: (ty_t, ty_f) :: (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
      let s4 = unify eqs in
      let res_t = subst_type s4 ty_t in (s4, res_t)
  | FunExp (id, body) ->
      let ty_arg = TyVar (fresh_tyvar()) in
      let newenv = Environment.extend id ty_arg tyenv in
      let (s, ty_body) = ty_exp newenv body in
      (s, TyFun (subst_type s ty_arg, ty_body))
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let newenv = Environment.extend id ty1 tyenv in
      let (s2, ty2) = ty_exp newenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | LetRecExp (id, para, exp1, exp2) ->
      let ty_para = TyVar (fresh_tyvar ()) in
      let ty_fun = TyFun (ty_para, TyVar (fresh_tyvar ())) in
      let funenv = Environment.extend id ty_fun (Environment.extend para ty_para tyenv) in
      let (s1, ty1) = ty_exp funenv exp1 in
      let inenv = Environment.extend id ty_fun tyenv in
      let (s2, ty2) = ty_exp inenv exp2 in
      let eqs = (ty_fun, TyFun (ty_para, ty1)) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | AppExp (exp1, exp2) ->
      let (s1, ty_fun) = ty_exp tyenv exp1 in
      let (s2, ty_arg) = ty_exp tyenv exp2 in
      let ty_ret = TyVar (fresh_tyvar ()) in
      let eqs = (ty_fun, TyFun (ty_arg, ty_ret)) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in 
      let s3 = unify eqs in (s3, subst_type s3 ty_ret)
  | MatchExp (exp_target, head, tail, exp_empty, exp_else) ->
      let (s1, ty_target) = ty_exp tyenv exp_target in
      let (s2, ty_empty) = ty_exp tyenv exp_empty in
      let tyv = TyVar (fresh_tyvar ()) in
      let (s3, ty_else) = ty_exp (Environment.extend head tyv (Environment.extend tail (TyList tyv) tyenv)) exp_else in
      let eqs = (ty_target, TyList tyv) :: (ty_empty, ty_else) :: (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
      let s4 = unify eqs in (s4, subst_type s4 ty_else)
  | LazyBinOp (op, exp1, exp2) ->
      (* TODO: nantokasuru *)
      ty_exp tyenv (BinOp (op, exp1, exp2))
  | _ -> err "ty_exp: Not implemented!"

let ty_decl tyenv = function
    Exp e -> let (_, ty) = ty_exp tyenv e in (tyenv, ty)
  | Decl (id, e) ->
      let (s, t) = ty_exp tyenv e in
      let s2 = unify (eqs_of_subst s) in
      let ty_ret = subst_type s2 t in (Environment.extend id ty_ret tyenv, ty_ret)
  | _ -> err "ty_decl: Not implemented!"
