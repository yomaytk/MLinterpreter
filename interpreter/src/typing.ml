open Syntax
exception Error of string

let err s = raise (Error s)
(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let rec sub_subst (numm, tyy) ty =
  match ty with
    TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (ty1, ty2) -> TyFun((sub_subst (numm, tyy) ty1), (sub_subst (numm, tyy) ty2))
  | TyVar num  -> if num = numm then tyy else TyVar num
  | TyList ty1 -> TyList ty1

let rec subst_type numtyl ty =
  match numtyl with
    [] -> ty
  | (num, tyy) :: rest -> subst_type rest (sub_subst (num, tyy) ty)

let rec map_subst numtyl tytyl =
  match tytyl with
    [] -> []
  | (ty1, ty2) :: rest -> ((subst_type numtyl ty1), (subst_type numtyl ty2)) :: (map_subst numtyl rest)

let rec research_ftv num ty =
  match ty with
    TyVar numm -> num = numm
  | TyFun (ty1, ty2) -> (research_ftv num ty1) || (research_ftv num ty2)
  | _ -> false

let rec unify tylist =
  match tylist with
    [] -> []
  | (t1, t2) :: rest ->
     if t1 = t2 then unify rest else begin
     (match t1, t2 with
         TyFun(ty11, ty12), TyFun(ty21, ty22) -> unify ((ty11, ty21) :: ((ty12, ty22) :: rest))
       | (TyVar num), ty1 ->
          (match research_ftv num ty1 with
              true -> err "unify error"
            | false -> unify ((map_subst [(num, ty1)] rest) @ [(TyVar num, ty1)]))
       | ty1, (TyVar num) ->
          (match research_ftv num ty1 with
             true -> err "unify error"
           | false -> unify ((map_subst [(num, ty1)] rest) @ [(TyVar num, ty1)]))
       | _ -> err "unify error")
     end

let pp_ty ty = print_string (string_of_ty ty)

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
  | ANDORBinOp (op, exp1, exp2) ->
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
