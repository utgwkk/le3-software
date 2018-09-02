open Syntax

exception Error of string

let err s = raise (Error s)

(* Type environment *)
type tyenv = tysc Environment.t

let rec freevar_tyenv tyenv =
  MySet.bigunion
    (Environment.fold_right
       (fun tysc set -> MySet.insert (freevar_tysc tysc) set)
       tyenv MySet.empty)


type subst = (tyvar * ty) list

let rec subst_type subs t =
  let rec substitute (tyvar, tyrepl) = function
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyList a -> TyList (substitute (tyvar, tyrepl) a)
    | TyVar a -> if a = tyvar then tyrepl else TyVar a
    | TyFun (a, b) ->
        TyFun (substitute (tyvar, tyrepl) a, substitute (tyvar, tyrepl) b)
    | TyTuple (a, b) ->
        TyTuple (substitute (tyvar, tyrepl) a, substitute (tyvar, tyrepl) b)
  in
  match subs with [] -> t | h :: ta -> subst_type ta (substitute h t)


let eqs_of_subst s = List.map (fun (tyv, t) -> (TyVar tyv, t)) s

let subst_eqs s eqs =
  List.map (fun (t1, t2) -> (subst_type s t1, subst_type s t2)) eqs


let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    MySet.bigunion
      (MySet.map (fun id -> freevar_ty (subst_type subst (TyVar id))) fv_tyenv')
  in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)


let rec unify =
  let rec ftv tyvar = function
    | TyFun (a, b) -> ftv tyvar a || ftv tyvar b
    | TyTuple (a, b) -> ftv tyvar a || ftv tyvar b
    | TyVar a -> tyvar = a
    | TyList a -> ftv tyvar a
    | _ -> false
  in
  function
    | [] -> []
    | (t1, t2) :: tl ->
        if t1 = t2 then unify tl
        else
          match (t1, t2) with
          | TyFun (t11, t12), TyFun (t21, t22) ->
              unify ((t11, t21) :: (t12, t22) :: tl)
          | TyTuple (t11, t12), TyTuple (t21, t22) ->
              unify ((t11, t21) :: (t12, t22) :: tl)
          | TyList t1, TyList t2 -> unify ((t1, t2) :: tl)
          | TyVar tv, t ->
              if ftv tv t then
                err ("type variable " ^ string_of_int tv ^ " appears.")
              else (tv, t) :: (unify @@ subst_eqs [(tv, t)] tl)
          | t, TyVar tv -> unify @@ (TyVar tv, t) :: tl
          | _ -> err "Type Unification Error"


let ty_prim op ty1 ty2 =
  match op with
  | Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Cons ->
      let a = TyVar (fresh_tyvar ()) in
      ([(ty1, a); (ty2, TyList a)], TyList a)


let rec ty_exp tyenv = function
  | Var x -> (
    try
      let TyScheme (vars, ty) = Environment.lookup x tyenv in
      let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
      ([], subst_type s ty)
    with Environment.Not_bound -> err ("Variable not bound: " ^ x) )
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | LLit l -> (
    match l with
    | [] -> ([], TyList (TyVar (fresh_tyvar ())))
    | h :: t -> ty_exp tyenv h )
  | BinOp (op, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let eqs3, ty = ty_prim op ty1 ty2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 @ eqs3 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty)
  | IfExp (exp_cond, exp_t, exp_f) ->
      let s1, ty_cond = ty_exp tyenv exp_cond in
      let s2, ty_t = ty_exp tyenv exp_t in
      let s3, ty_f = ty_exp tyenv exp_f in
      let eqs =
        (ty_cond, TyBool) :: (ty_t, ty_f) :: eqs_of_subst s1 @ eqs_of_subst s2
        @ eqs_of_subst s3
      in
      let s4 = unify eqs in
      let res_t = subst_type s4 ty_t in
      (s4, res_t)
  | FunExp (id, body) ->
      let ty_arg = TyVar (fresh_tyvar ()) in
      let newenv = Environment.extend id (tysc_of_ty ty_arg) tyenv in
      let s, ty_body = ty_exp newenv body in
      (s, TyFun (subst_type s ty_arg, ty_body))
  | DFunExp (id, body) ->
      (* TODO: nantokasuru *)
      ty_exp tyenv (FunExp (id, body))
  | LetExp (id, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let tysc1 = closure ty1 tyenv s1 in
      let newenv = Environment.extend id tysc1 tyenv in
      let s2, ty2 = ty_exp newenv exp2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)
  | LetRecExp (id, para, exp1, exp2) ->
      let ty_para = TyVar (fresh_tyvar ()) in
      let ty_fun = TyFun (ty_para, TyVar (fresh_tyvar ())) in
      let funenv =
        Environment.extend id (tysc_of_ty ty_fun)
          (Environment.extend para (tysc_of_ty ty_para) tyenv)
      in
      let s1, ty1 = ty_exp funenv exp1 in
      let tysc = closure (subst_type s1 (TyFun (ty_para, ty1))) tyenv s1 in
      let inenv = Environment.extend id tysc tyenv in
      let s2, ty2 = ty_exp inenv exp2 in
      let eqs =
        (ty_fun, TyFun (ty_para, ty1)) :: eqs_of_subst s1 @ eqs_of_subst s2
      in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)
  | AppExp (exp1, exp2) ->
      let s1, ty_fun = ty_exp tyenv exp1 in
      let s2, ty_arg = ty_exp tyenv exp2 in
      let ty_ret = TyVar (fresh_tyvar ()) in
      let eqs =
        (ty_fun, TyFun (ty_arg, ty_ret)) :: eqs_of_subst s1 @ eqs_of_subst s2
      in
      let s3 = unify eqs in
      (s3, subst_type s3 ty_ret)
  | MatchExp (exp_target, head, tail, exp_empty, exp_else) ->
      let s1, ty_target = ty_exp tyenv exp_target in
      let s2, ty_empty = ty_exp tyenv exp_empty in
      let tyv = TyVar (fresh_tyvar ()) in
      let s3, ty_else =
        ty_exp
          (Environment.extend head (tysc_of_ty tyv)
             (Environment.extend tail (tysc_of_ty (TyList tyv)) tyenv))
          exp_else
      in
      let eqs =
        (ty_target, TyList tyv) :: (ty_empty, ty_else) :: eqs_of_subst s1
        @ eqs_of_subst s2 @ eqs_of_subst s3
      in
      let s4 = unify eqs in
      (s4, subst_type s4 ty_else)
  | LoopExp (id, exp1, exp2) ->
      let loop_letrec =
        LetRecExp ("__loop__", id, exp2, AppExp (Var "__loop__", exp1))
      in
      ty_exp tyenv loop_letrec
  | RecurExp e ->
      let loop_recur = AppExp (Var "__loop__", e) in
      ty_exp tyenv loop_recur
  | TupleExp (exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 (TyTuple (ty1, ty2)))
  | ProjExp (exp, idx) -> (
      let s1, ty_tup = ty_exp tyenv exp in
      let tyv_left = TyVar (fresh_tyvar ()) in
      let tyv_right = TyVar (fresh_tyvar ()) in
      let eqs = [(ty_tup, TyTuple (tyv_left, tyv_right))] @ eqs_of_subst s1 in
      let s2 = unify eqs in
      match idx with
      | 1 -> (s2, subst_type s2 tyv_left)
      | 2 -> (s2, subst_type s2 tyv_right)
      | _ -> err "ty_exp: projection index out of range" )
  | LazyBinOp (op, exp1, exp2) -> ty_exp tyenv (BinOp (op, exp1, exp2))


let ty_decl tyenv = function
  | Exp e ->
      let _, ty = ty_exp tyenv e in
      (tyenv, ty)
  | Decl (id, e) ->
      let s, t = ty_exp tyenv e in
      let tysc = closure t tyenv s in
      let newenv = Environment.extend id tysc tyenv in
      let s2 = unify (eqs_of_subst s) in
      let ty_ret = subst_type s2 t in
      (newenv, ty_ret)
  | RecDecl (id, para, body) ->
      let ty_para = TyVar (fresh_tyvar ()) in
      let ty_fun = TyFun (ty_para, TyVar (fresh_tyvar ())) in
      let funenv =
        Environment.extend id (tysc_of_ty ty_fun)
          (Environment.extend para (tysc_of_ty ty_para) tyenv)
      in
      let s, t = ty_exp funenv body in
      let eqs = (ty_fun, t) :: eqs_of_subst s in
      let s2 = unify eqs in
      let ty_ret = subst_type s2 t in
      let tysc = closure ty_ret tyenv s2 in
      (Environment.extend id tysc tyenv, ty_ret)
