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
%token FPLUS FMULT
%token WHAT

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=RecLetExpr SEMISEMI { e }
  /* | e=LetExpr SEMISEMI { e } */
  | e=LetAndExpr SEMISEMI { e }
  | e=LetFunExpr SEMISEMI { e }
  /* | e=WExpr SEMISEMI { ParseFail } */

Expr :
    e=IfExpr { e }
  | e=LetInExpr { e }
  /* | e=ORExpr { e } */
  /* | e=BExpr { e } */
  | e=FPlusExpr { e }
  | e=FunExpr { e }
  /* | e=FunMExpr { e } */
  | e=LetAndInExpr { e }

/* WExpr :
    e=Expr { e }
  | e1=WExpr WHAT e2=WExpr { e1 }
  | e=WExpr WHAT { e }
  | WHAT e=WExpr { e }
  | WHAT { BLit false } */

FPlusExpr :
    FPLUS x1=AExpr x2=AExpr { BinOp (Plus, x1, x2) }
  | FPLUS x1=ID x2=ID { FplmuBinOp (Plus, x1, x2) }
  | FPLUS x1=AExpr { FplmuFunExp (Plus, x1, "-") }
  | FPLUS { FplmuFunExp (Plus, ILit 0, "--") }
  | e=FMultExpr { e }

FMultExpr :
    FMULT x1=AExpr x2=AExpr { BinOp (Mult, x1, x2) }
  | e=BExpr { e }

BExpr :
    l=LTExpr AAND r=Expr { ANDORBinOp(AAND, l, r) }
  | l=LTExpr OOR r=Expr { ANDORBinOp(OOR, l, r) }
  | e=ORExpr { e }

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
    FUN x=ID RARROW e=Expr { FunExp (x, e) }

/* FunMExpr :
    FUN x=ID e=FunMExpr { FunExp (x, e) }
  (* | x=ID e=FunMExpr { FunExp (x, e) } *)
  | x=ID RARROW e=Expr { FunExp (x, e) } */

LetFunExpr :
    LET x=ID e=LetFunFunExpr { Decl (x, e) }

LetFunFunExpr :
    /* x=ID e=LetFunFunExpr { FunExp (x, e) } */
  | x=ID EQ e=Expr { FunExp (x, e) }

LetAndExpr :
    LET x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) }

AndExpr :
    x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) }
  | x=ID EQ e=Expr { Decl (x, e) }

LetAndInExpr :
    LET x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr IN e2=Expr { LetEndInExp (x, e1, e2) }

