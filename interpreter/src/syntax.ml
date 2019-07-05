open MySet
(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | AAND | OOR | Cons

exception Error of string

let err s = raise (Error s)

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

let counter = ref 0

let tyvarlist = ref []
          
let makelist nextn =
    let nexts = "'a"^(string_of_int nextn) in
    tyvarlist := (nextn, nexts) :: !tyvarlist; nexts

let rec researchlist cnt list =
  match list with
        [] -> err "researchlist error"
      | (n, id) :: rest -> if n=cnt then id else researchlist cnt rest

let fresh_tyvar =
  let body () =
  let v = !counter in
    counter := v + 1; v
  in body

let rec string_of_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar num -> if num = !counter then (makelist (fresh_tyvar ())) else researchlist num !tyvarlist
  | TyFun (ty1, ty2) -> "(" ^ (string_of_ty ty1) ^ " -> " ^ (string_of_ty ty2) ^ ")"
  | _ -> "error"

let rec freevar_ty ty =
  match ty with
    TyVar num -> insert num MySet.empty
  | TyFun (ty1, ty2) -> join (freevar_ty ty1) (freevar_ty ty2)
  | _ -> MySet.empty
