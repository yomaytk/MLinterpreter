(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | AAND | OOR

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | NIlV
  | BinOp of binOp * exp * exp
  | ANDORBinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetInExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetAndInExp of id * exp * exp
  | LetEndInExp of id * exp * exp
  | FplmuBinOp of binOp * id * id
  | FplmuFunExp of binOp * exp * id
  | DfunExp of id * exp
  | LetRecExp of id * id * exp * exp
  | ListExp of exp * exp
  | ListFirstExp of exp

type program =
    Exp of exp
  | Decl of id * exp
  | DecDecl of id * exp * program (*letの並列宣言にマッチする*)
  | AndLet of id * exp * program
  | RecDecl of id * id * exp
  | RecAndLet of id * id * exp * program
  | ParseFail 

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty
