(* ML interpreter / type reconstruction *)
type ty =
    TyInt
  | TyBool

let pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"

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
