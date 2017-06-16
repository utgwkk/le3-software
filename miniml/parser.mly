%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT LT AND OR
%token LET REC IN EQ
%token IF THEN ELSE TRUE FALSE
%token RARROW FUN DFUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET x=ID e=LetFunExpr SEMISEMI { Decl (x, e) }
  | LET REC x=ID EQ FUN para=ID RARROW body=Expr SEMISEMI { RecDecl (x, para, FunExp(para, body)) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }
  | e=DFunExpr { e }
  | e=LetRecExpr { e }

FunExpr :
    FUN body=FunBodyExpr { body }

DFunExpr :
    DFUN x=ID RARROW body=Expr { DFunExp (x, body) }

FunBodyExpr :
    x=ID RARROW body=Expr { FunExp (x, body) }
  | x=ID e=FunBodyExpr { FunExp (x, e)}

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
  | LET id=ID e1=LetFunExpr IN e2=Expr { LetExp (id, e1, e2) }

LetFunExpr :
    id=ID EQ body=Expr { FunExp (id, body) }
  | id=ID e=LetFunExpr { FunExp (id, e) }

LetRecExpr :
    LET REC x=ID EQ FUN para=ID RARROW body=Expr IN e=Expr { LetRecExp (x, para, body, e) }

OrExpr :
    l=OrExpr OR r=AndExpr { LazyBinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=AndExpr AND r=CmpExpr { LazyBinOp (And, l, r) }
  | e=CmpExpr { e }

CmpExpr :
    e=LTExpr { e }
  | e=EQExpr { e }
  | e=PExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }

EQExpr :
    l=PExpr EQ r=PExpr { BinOp (Eq, l, r) }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | l=PExpr MINUS r=MExpr { BinOp (Minus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=BiOper RPAREN { e }
  | LPAREN e=Expr RPAREN { e }

BiOper :
    PLUS { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Plus, Var ("__lhs__"), Var ("__rhs__")))) }
  | MINUS { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Minus, Var ("__lhs__"), Var ("__rhs__")))) }
  | MULT { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Mult, Var ("__lhs__"), Var ("__rhs__")))) }
  | LT { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Lt, Var ("__lhs__"), Var ("__rhs__")))) }
  | EQ { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Eq, Var ("__lhs__"), Var ("__rhs__")))) }
  | AND { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (And, Var ("__lhs__"), Var ("__rhs__")))) }
  | OR { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Or, Var ("__lhs__"), Var ("__rhs__")))) }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
