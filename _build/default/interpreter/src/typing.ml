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


(* eqs_of_subst : subst -> (ty * ty) list 型代入を型の等式集合に変換*)
let rec eqs_of_subst (s : (tyvar * ty) list) = 
  match s with
      [] -> []
    | (num, ty2) :: rest -> (TyVar num, ty2) :: (eqs_of_subst rest)

(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list 型の等式集合に型代入を適用*)
let subst_eqs s eqs = map_subst s eqs

let rec unify tylist =
  match tylist with
    [] -> []
  | (t1, t2) :: rest ->
      if t1 = t2 then unify rest else begin
      (match t1, t2 with
          TyFun(ty11, ty12), TyFun(ty21, ty22) -> unify ((ty11, ty21) :: ((ty12, ty22) :: rest))
        | TyVar num, ty1 ->
            (match research_ftv num ty1 with
                true -> err "unify error"
              | false -> (num, ty1) :: (unify (map_subst [(num, ty1)] rest)))
        | ty1, TyVar num ->
          (match research_ftv num ty1 with
              true -> err "unify error"
            | false -> (num, ty1) :: (unify (map_subst [(num, ty1)] rest)))
        | _ -> err "unify error")
      end

let pp_ty ty = print_string (string_of_ty ty)

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)

  | AAND -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

  | OOR -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

  | Cons -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
        Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | ANDORBinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs4 = (ty1, TyBool) in
      let eqs5 = (ty2, ty3)  in
      let eqs = eqs4 :: (eqs5 :: ((eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3))) in 
      let us = unify eqs in (us, subst_type us ty3)
      (* (match ty1 with
        TyBool -> 
          (let (s2, ty2) = ty_exp tyenv exp2 in
            let (s3, ty3) = ty_exp tyenv exp3 in 
            (match (ty2 = ty3) with
                true -> 
                  (let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
                    let s4 = unify eqs in (s4, subst_type s4 ty2))
              | _ -> err "error of if typing"))
      | _ -> err "error of if typing") *)
  | LetInExp (id, exp1, exp2) ->
      let domty = TyVar (fresh_tyvar ()) in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tynewenv = Environment.extend id domty tyenv in
      let (s2, ty2) = ty_exp tynewenv exp2 in
      let eqs3 = [(domty, ty1)] in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      (match ty1 with
          TyFun(tyy1, tyy2) ->
            (* pp_ty ty1;print_string "why"; *)
            let eqs3 = [(tyy1, ty2)] in
            let eqs4 = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
            let s5 = unify eqs4 in (s5, subst_type s5 tyy2)
        | TyVar num -> 
            let tyr = TyVar (fresh_tyvar ()) in
            let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(TyVar num, TyFun(ty2, tyr))] in
            let s6 = unify eqs in  (s6, subst_type s6 tyr)
        | _ -> pp_ty ty1;err "error AppExp typing")

  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")