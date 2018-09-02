(* ML interpreter / type reconstruction *)
type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyTuple of ty * ty
  | TyList of ty

let rec pp_ty t =
  let rec collect_tyvar set = function
    | TyVar i -> MySet.singleton i
    | TyFun (a, b) -> MySet.union (collect_tyvar set b) (collect_tyvar set a)
    | TyTuple (a, b) -> MySet.union (collect_tyvar set b) (collect_tyvar set a)
    | _ -> MySet.empty
  in
  let rec tyvar_map count = function
    | [] -> []
    | h :: t -> (h, "t" ^ string_of_int count) :: tyvar_map (count + 1) t
  in
  let tyvars = tyvar_map 0 (MySet.to_list @@ collect_tyvar MySet.empty t) in
  let rec pp_ty' t =
    match t with
    | TyInt -> print_string "int"
    | TyBool -> print_string "bool"
    | TyVar i -> (
      try print_string @@ "'" ^ List.assoc i tyvars with Not_found -> pp_ty t )
    | TyList a -> pp_ty' a ; print_string " list"
    | TyTuple (a, b) -> (
      match (a, b) with
      | TyTuple _, _ ->
          print_string "(" ;
          pp_ty' a ;
          print_string ")" ;
          print_string " * " ;
          pp_ty' b
      | _, TyTuple _ ->
          pp_ty' a ;
          print_string " * " ;
          print_string "(" ;
          pp_ty' b ;
          print_string ")"
      | _, TyFun _ ->
          pp_ty' a ;
          print_string " * " ;
          print_string "(" ;
          pp_ty' b ;
          print_string ")"
      | _ -> pp_ty' a ; print_string " * " ; pp_ty' b )
    | TyFun (a, b) ->
      match (a, b) with
      | TyFun _, _ ->
          print_string "(" ;
          pp_ty' a ;
          print_string ")" ;
          print_string " -> " ;
          pp_ty' b
      | _ -> pp_ty' a ; print_string " -> " ; pp_ty' b
  in
  pp_ty' t


let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1 ;
    v
  in
  body


let rec freevar_ty ty =
  match ty with
  | TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyList a -> freevar_ty a
  | TyVar a -> MySet.singleton a
  | TyFun (a, b) -> MySet.union (freevar_ty a) (freevar_ty b)
  | TyTuple (a, b) -> MySet.union (freevar_ty a) (freevar_ty b)


type id = string

type binOp = Plus | Mult | Lt | And | Or | Cons

type exp =
  | Var of id
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
  | LoopExp of id * exp * exp
  | RecurExp of exp
  | TupleExp of exp * exp
  | ProjExp of exp * int

type program = Exp of exp | Decl of id * exp | RecDecl of id * id * exp

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)

let freevar_tysc (TyScheme (tyvars, ty)) =
  MySet.diff (freevar_ty ty) (MySet.from_list tyvars)
