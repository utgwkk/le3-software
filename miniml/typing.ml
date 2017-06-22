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
  | TyVar a -> if a = tyvar then tyrepl else TyVar a
  | TyFun (a, b) -> TyFun ((substitute (tyvar, tyrepl) a), (substitute (tyvar, tyrepl) b))
  in
  match subs with
    [] -> t
  | h::ta -> subst_type ta (substitute h t)

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
        TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: +"))
  | Minus -> (match ty1, ty2 with
        TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: -"))
  | Mult -> (match ty1, ty2 with
        TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with
        TyInt, TyInt -> TyBool
      | _ -> err ("Argument must be of integer: <"))
  | Eq -> (match ty1, ty2 with
        TyInt, TyInt -> TyBool
      | _ -> err ("Argument must be of integer: ="))
  | And -> (match ty1, ty2 with
        TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of boolean: &&"))
  | Or -> (match ty1, ty2 with
        TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of boolean: ||"))
  | _ -> err "Not implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try Environment.lookup x tyenv with
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  | IfExp (exp_cond, exp_t, exp_f) ->
      let tyarg_cond = ty_exp tyenv exp_cond in (
      if tyarg_cond = TyBool then
          let tyarg_t = ty_exp tyenv exp_t in
          let tyarg_f = ty_exp tyenv exp_f in
            if tyarg_t = tyarg_f then tyarg_t
            else err "If-then and else must be the same type"
      else err "Condition must be boolean"
    )
  | LetExp (id, exp1, exp2) ->
      let var_type = ty_exp tyenv exp1 in
      ty_exp (Environment.extend id var_type tyenv) exp2
  | LazyBinOp (op, exp1, exp2) ->
      (* TODO: nantokasuru *)
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  | _ -> err "Not implemented!"

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err "Not implemented!"
