open Syntax
exception Error of string

let err s = raise (Error s)
(* Type Environment *)
type tyenv = ty Environment.t

let pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | _ -> print_string "error"

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
              TyInt, TyInt -> TyInt
            | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with
              TyInt, TyInt -> TyInt
            | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with
            TyInt, TyInt -> TyBool
          | _ -> err ("Argument must be of integer: <"))
  | AAND -> (match ty1, ty2 with
              TyBool, TyBool -> TyBool
            | _ -> err ("Argument must be of boolean: &&"))
  | OOR -> (match ty1, ty2 with
              TyBool, TyBool -> TyBool
            | _ -> err ("Argument must be of boolean: ||"))
  | Cons -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try Environment.lookup x tyenv with
        Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let tyarg1 = ty_exp tyenv exp1 in
      (match tyarg1 with
        TyBool -> (let tyarg2 = ty_exp tyenv exp2 in
                    let tyarg3 = ty_exp tyenv exp3 in (match (tyarg2 = tyarg3) with
                                                        true -> tyarg2
                                                      | _ -> err "error of if typing"))
      | _ -> err "error of if typing")
  | LetInExp (id, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
        let tynewenv = Environment.extend id tyarg1 tyenv in
        ty_exp tynewenv exp2
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")