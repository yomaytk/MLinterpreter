%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token AMPERAMPER PAIPUPAIPU
%token FUN RARROW
%token AND
%token WHAT

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=RecLetExpr SEMISEMI { e }
  | e=LetExpr SEMISEMI { e }
  | e=LetAndExpr SEMISEMI { e }
  | WHAT { Rongai }
  | SEMISEMI { Rongai }

Expr :
    e=IfExpr { e }
  | e=LetInExpr { e }
  | e=BExpr { e }
  | e=FunExpr { e }
  | e=LetAndInExpr { e }

BExpr :
    l=LTExpr AMPERAMPER r=BExpr { BinOp(AMPERAMPER, l, r) }
  | l=LTExpr PAIPUPAIPU r=BExpr { BinOp(PAIPUPAIPU, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    e1=MExpr MULT e2=AExpr { BinOp (Mult, e1, e2) }
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

LetInExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetInExp (x, e1, e2) }

LetExpr : 
    LET x=ID EQ e=Expr { Decl (x, e) }

RecLetExpr :
    LET x=ID EQ e1=Expr e2=RecLetExpr { RecDecl (x, e1, e2) }
  | e=LetExpr { e }

FunExpr : 
  FUN e1=ID RARROW e2=Expr { FunExp (e1, e2) }

LetAndExpr :
    LET x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) }

AndExpr :
    x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) }
  | x=ID EQ e=Expr { Decl (x, e) }

LetAndInExpr :
    LET x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr IN e2=Expr { LetEndInExp (x, e1, e2) }

