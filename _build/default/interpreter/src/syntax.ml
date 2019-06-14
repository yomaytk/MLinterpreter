(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | AAND | OOR

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | ANDORBinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetInExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetAndInExp of id * exp * exp
  | LetEndInExp of id * exp * exp

type program =
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * exp * program
  | AndLet of id * exp * program
  | Rongai

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty
