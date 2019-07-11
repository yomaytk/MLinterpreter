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

(* type scheme *)
type tysc = TyScheme of tyvar list * ty

let counter = ref 0

let tyvarlist = ref []

let addlist nextn =
  let nexts = "'a"^(string_of_int nextn) in
  tyvarlist := (nextn, nexts) :: !tyvarlist; nexts

let rec researchlist cnt list =
  match list with
      [] -> print_string(string_of_int cnt);"error reserarchlist"
    | (n, id) :: rest -> if n=cnt then id else researchlist cnt rest

let fresh_tyvar =
  let body () =
  let v = !counter in
    counter := v + 1; ignore(addlist v);v
  in body

let rec mkmktyid num = 
  if num = !counter then addlist (fresh_tyvar())
  else if num > !counter then begin ignore(addlist (fresh_tyvar()));mkmktyid num end
  else err "mkmktyid error"

let rec string_of_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar num -> if num >= !counter then mkmktyid num else researchlist num !tyvarlist
  | TyFun (ty1, ty2) -> "(" ^ (string_of_ty ty1) ^ " -> " ^ (string_of_ty ty2) ^ ")"
  | _ -> "error"

let rec freevar_ty ty =
  match ty with
    TyVar num -> insert num MySet.empty
  | TyFun (ty1, ty2) -> union (freevar_ty ty1) (freevar_ty ty2)
  | _ -> MySet.empty

let tysc_of_ty ty = TyScheme ([], ty)

let rec freevar_tysc tyscheme =
  match tyscheme with
      TyScheme (tyvarlist, ty1) ->
        (match ty1 with
            TyVar num -> (if not (List.mem num tyvarlist) then [num] else [])
          | TyFun(tyy1, tyy2) -> (freevar_tysc (TyScheme(tyvarlist, tyy1))) @ (freevar_tysc (TyScheme(tyvarlist, tyy2)))
          | _ -> [])
