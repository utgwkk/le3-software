%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token LET REC IN EQ
%token IF THEN ELSE TRUE FALSE
%token RARROW FUN DFUN
%token CONS LLPAREN RLPAREN SEMI
%token MATCH WITH PIPE
%token LOOP RECUR
%token DOT COMMA

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
    e=OrExpr { e }
  | e=LoopExpr { e }

ExpandExpr :
    e=LetExpr { e }
  | e=FunExpr { e }
  | e=DFunExpr { e }
  | e=LetRecExpr { e }
  | e=MatchWithExpr { e }

MatchWithExpr :
    MATCH target=Expr WITH
    LLPAREN RLPAREN RARROW e1=Expr
    PIPE head=ID CONS tail=ID RARROW e2=Expr { MatchExp (target, head, tail, e1, e2) }
  | e=IfExpr { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LoopExpr :
    LOOP x=ID EQ e1=Expr IN e2=Expr { LoopExp (x, e1, e2) }

FunExpr :
    FUN body=FunBodyExpr { body }

DFunExpr :
    DFUN x=ID RARROW body=Expr { DFunExp (x, body) }

FunBodyExpr :
    x=ID RARROW body=Expr { FunExp (x, body) }
  | x=ID e=FunBodyExpr { FunExp (x, e)}

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (LVar x, e1, e2) }
  | LET LPAREN x=ID COMMA y=ID RPAREN EQ e1=Expr IN e2=Expr { LetExp (LTuple (x, y), e1, e2) }
  | LET id=ID e1=LetFunExpr IN e2=Expr { LetExp (LVar id, e1, e2) }

LetFunExpr :
    id=ID EQ body=Expr { FunExp (id, body) }
  | id=ID e=LetFunExpr { FunExp (id, e) }

LetRecExpr :
    LET REC x=ID EQ FUN para=ID RARROW body=Expr IN e=Expr { LetRecExp (x, para, body, e) }

OrExpr :
    l=OrExpr OR r=AndExpr { LazyBinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=AndExpr AND r=LTExpr { LazyBinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr : 
    l=ConsExpr LT r=ConsExpr { BinOp (Lt, l, r) }
  | e=ConsExpr { e }

ConsExpr :
    l=PExpr CONS r=ConsExpr { BinOp (Cons, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | RECUR e=AExpr { RecurExp e }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | LLPAREN RLPAREN { LLit [] }
  | LLPAREN e=ListExpr RLPAREN { e }
  | i=ID   { Var i }
  | LPAREN e=BiOper RPAREN { e }
  | LPAREN e=Expr RPAREN { e }
  | LPAREN e1=Expr COMMA e2=Expr RPAREN { TupleExp (e1, e2) }
  | e=AExpr DOT i=INTV { ProjExp (e, i) }
  | e=ExpandExpr { e }

ListExpr :
    e=Expr SEMI l=ListExpr { BinOp (Cons, e, l) }
  | e=Expr SEMI { BinOp (Cons, e, LLit []) }
  | e=Expr { BinOp (Cons, e, LLit []) }

BiOper :
    PLUS { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Plus, Var ("__lhs__"), Var ("__rhs__")))) }
  | MULT { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Mult, Var ("__lhs__"), Var ("__rhs__")))) }
  | LT { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Lt, Var ("__lhs__"), Var ("__rhs__")))) }
  | AND { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (And, Var ("__lhs__"), Var ("__rhs__")))) }
  | OR { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Or, Var ("__lhs__"), Var ("__rhs__")))) }
  | CONS { FunExp ("__lhs__", FunExp ("__rhs__", BinOp (Cons, Var ("__lhs__"), Var ("__rhs__")))) }
