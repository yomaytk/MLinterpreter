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
%token REC
%token MDLPAREN MDRPAREN
%token SEMI COROCORO
%token DFUN
%token MATCH WITH PAIPU

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
		e=Expr SEMISEMI { Exp e }
	| e=DecLetExpr SEMISEMI { e }
	| LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e) }
	| LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr AND e2=RecAndExpr SEMISEMI { RecAndLet (x1, x2, e1, e2) }
	| LET x=ID EQ e1=Expr AND e2=AndExpr SEMISEMI { AndLet (x, e1, e2) }
	| e=LetFunExpr SEMISEMI { e }

Expr :
		e=LetRecExpr { e }  
	| e=IfExpr { e } 
	| e=LetInExpr { e }
	| e=BExpr { e }
	| e=FunExpr { e }
	| e=FunMExpr { e }
	| e=LetAndInExpr { e }
	| e=RecAndInExpr { e }
	| e=ListCoroExpr { e }
	| e=MatchExpr { e }

BExpr :
		l=LTExpr AAND r=Expr { ANDORBinOp(AAND, l, r) } (*false && undef の判定に利用する*)
	| l=LTExpr OOR r=Expr { ANDORBinOp(OOR, l, r) }   (*true || undef の判定に利用する*)
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
		e1=MExpr MULT e2=ListCoroExpr { BinOp (Mult, e1, e2) }
	| e=ListCoroExpr { e }

ListCoroExpr :
		e1=ListSingleExpr COROCORO e2=ListCoroExpr { ListExp (e1, e2) }
	| MDRPAREN e1=ListCoroExpr SEMI e2=ListInExpr { ListExp (e1, e2) }
	| e=ListSingleExpr { e }

ListInExpr :
		e1=Expr SEMI e2=ListInExpr { ListExp (e1, e2) }
	| e=Expr MDLPAREN { ListExp (e, NIlV) }

ListSingleExpr :
		MDRPAREN e=AExpr MDLPAREN { ListExp(e, NIlV) }
	| e=AppExpr { e }

AppExpr : 
		e1=AppExpr e2=FPlusFunExpr { AppExp (e1, e2) }
	| e1=AppExpr FPLUS e2=AExpr { AppExp(AppExp(e1, FplmuFunExp (Plus, ILit 0, "--")), e2) }
	| e1=AppExpr FMULT e2=AExpr { AppExp(AppExp(e1, FplmuFunExp (Mult, ILit 0, "--")), e2) }
	| e=FPlusFunExpr { e }

FPlusFunExpr :
		FPLUS { FplmuFunExp (Plus, ILit 0, "--") }
	| FPLUS e=AExpr { FplmuFunExp (Plus, e, "-") }
	| FPLUS e1=AExpr e2=AExpr { BinOp (Plus, e1, e2) }
	| e=FMultFunExpr { e }

FMultFunExpr :
		FMULT { FplmuFunExp (Mult, ILit 0, "--") }
	| FMULT e=AExpr { FplmuFunExp (Mult, e, "-") }
	| FMULT e1=AExpr e2=AExpr { BinOp (Mult, e1, e2) }
	| e=AExpr { e }

AExpr :
		i=INTV { ILit i }
	| TRUE   { BLit true }
	| FALSE  { BLit false }
	| i=ID   { Var i }
	| MDRPAREN MDLPAREN { NIlV }
	| LPAREN e=Expr RPAREN { e }

IfExpr :
	IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetInExpr :
	LET x=ID EQ e1=Expr IN e2=Expr { LetInExp (x, e1, e2) }

LetExpr : 
	LET x=ID EQ e=Expr { Decl (x, e) }

DecLetExpr :
		LET x=ID EQ e1=Expr e2=DecLetExpr { DecDecl (x, e1, e2) } (*letの並列宣言にマッチする*)
	| e=LetExpr { e }

FunExpr : 
		FUN x=ID RARROW e=Expr { FunExp (x, e) }
	| DFUN x=ID RARROW e=Expr { DfunExp (x, e) }

FunMExpr :
		FUN x=ID e=FunMExpr { FunExp (x, e) }
  	/* | x=ID e=FunMExpr { FunExp (x, e) } */
	| x=ID RARROW e=AppExpr { FunExp (x, e) }

LetFunExpr :
	LET x=ID e=LetFunFunExpr { Decl (x, e) }

LetFunFunExpr :
		x=ID e=LetFunFunExpr { FunExp (x, e) }
	| x=ID EQ e=Expr { FunExp (x, e) }

AndExpr :
		x=ID EQ e1=Expr AND e2=AndExpr { AndLet (x, e1, e2) } (*let and並列宣言のand以降の宣言にマッチする*)
	| x=ID EQ e=Expr { Decl (x, e) }

LetAndInExpr :
		LET x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }  (*let and並列宣言の一行目にマッチする*)
	| x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) } (*let and並列宣言の途中の宣言にマッチする*)
	| x=ID EQ e1=Expr IN e2=Expr { LetEndInExp (x, e1, e2) } (*let and並列宣言の最後の宣言にマッチする*)

LetRecExpr :
	LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1, x2, e1, e2) }

RecAndExpr :
		x1=ID EQ FUN x2=ID RARROW e1=Expr AND e2=RecAndExpr { RecAndLet (x1, x2, e1, e2) }
	| x1=ID EQ FUN x2=ID RARROW e=Expr { RecDecl (x1, x2, e) }

RecAndInExpr :
		LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr AND e2=RecAndInExpr { LetRecExp (x1, x2, e1, e2) }
	| x1=ID EQ FUN x2=ID RARROW e1=Expr AND e2=RecAndInExpr { LetRecExp (x1, x2, e1, e2) }
	| x=ID EQ e1=Expr IN e2=Expr { LetEndInExp (x, e1, e2) }

MatchExpr :
	MATCH e1=Expr WITH MDRPAREN MDLPAREN RARROW e2=Expr PAIPU id1=ID COROCORO id2=ID RARROW e3=Expr { MatchExp(e1, e2, id1, id2, e3) }
