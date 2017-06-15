%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token LET IN EQ
%token IF THEN ELSE TRUE FALSE
%token RARROW FUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }

FunExpr :
    FUN x=ID RARROW body=Expr { FunExp (x, body) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

OrExpr :
    l=OrExpr OR r=Expr { BinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=AndExpr AND r=Expr { BinOp (And, l, r) }
  | e=CmpExpr { e }

CmpExpr :
    e=LTExpr { e }
  | e=EQExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

EQExpr :
    l=PExpr EQ r=PExpr { BinOp (Eq, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
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
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
