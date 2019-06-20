%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token AAND OOR
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
  | e=LetAndExpr SEMISEMI { e }
  | e=WExpr SEMISEMI { ParseFail }

Expr :
    e=IfExpr { e }
  | e=LetInExpr { e }
  | e=ORExpr { e }
  | e=BExpr { e }
  | e=FunExpr { e }
  | e=LetAndInExpr { e }

WExpr :
    e=Expr { e }
  | e1=WExpr WHAT e2=WExpr { e1 }
  | e=WExpr WHAT { e }
  | WHAT e=WExpr { e }
  | WHAT { BLit false }

BExpr :
    l=LTExpr AAND r=Expr { ANDORBinOp(AAND, l, r) }
  | l=LTExpr OOR r=Expr { ANDORBinOp(OOR, l, r) }
  | e=LTExpr { e }

ORExpr :
    l=ORExpr OOR r=ANDExpr { BinOp (OOR, l, r) }
  | e=ANDExpr { e }

ANDExpr : 
    l=ANDExpr AAND r=ORExpr { BinOp (AAND, l, r) }
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
    e1=AppExpr e2=MultAppExpr { AppExp (e1, e2) }
  | e=AExpr { e }

MultAppExpr :
    e1=AExpr e2=MultAppExpr { e1::e2 }
  | e=AExpr { [e] }

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
    FUN e1=MultFunExpr RARROW e2=Expr { FunExp (e1, e2) }
  /* | FUN e1=ID e2=MultFunExpr RARROW e3=Expr { FunExp (e1::e2, e3)} */
  
MultFunExpr :
    e1=ID e2=MultFunExpr { e1::e2 }
  | e=ID { [e] }

LetAndExpr :
    LET x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) }

AndExpr :
    x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) }
  | x=ID EQ e=Expr { Decl (x, e) }

LetAndInExpr :
    LET x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr IN e2=Expr { LetEndInExp (x, e1, e2) }

