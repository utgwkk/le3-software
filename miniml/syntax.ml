(* ML interpreter / type reconstruction *)
type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar _ -> print_string "(unknown)"
  | TyFun (a, b) ->
      pp_ty a;
      print_string " -> ";
      pp_ty b

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
      counter := v + 1; v
  in body

let rec freevar_ty ty = match ty with
    TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyVar a -> MySet.singleton a
  | TyFun (a, b) -> MySet.union (freevar_ty a) (freevar_ty b)

type id = string

type binOp = Plus | Minus | Mult | Lt | Eq | And | Or | Cons

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | LLit of exp list
  | BinOp of binOp * exp * exp
  | LazyBinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp
  | MatchExp of exp * id * id * exp * exp

type program = 
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp
