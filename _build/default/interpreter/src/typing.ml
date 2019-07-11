open Syntax
exception Error of string

let err s = raise (Error s)
(* Type Environment *)
type tyenv = tysc Environment.t

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

let rec freevar_tyenv tyenv = 
  let schemelist = Environment.getschemelist tyenv in
  let rec freevar_tyenvrec tyenv2 = 
    match tyenv2 with
      [] -> MySet.empty
    | TyScheme(tyvarlist, tyy) :: rest -> MySet.insertlist (freevar_tyenvrec rest) (freevar_tysc (TyScheme(tyvarlist, tyy)))
  in freevar_tyenvrec schemelist

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
  MySet.bigunion
    (MySet.map
      (fun id -> freevar_ty (subst_type subst (TyVar id)))
        fv_tyenv') in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)

  | AAND -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

  | OOR -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

  | Cons -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try 
        let TyScheme (vars, ty) = Environment.lookup x tyenv in
        let s = List.map (fun v -> (v, TyVar (fresh_tyvar ())))
        vars in
        (tyenv, [], subst_type s ty)
        with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> (tyenv, [], TyInt)
  | BLit _ -> (tyenv, [], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (_, s1, ty1) = ty_exp tyenv exp1 in
      let (_, s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (tyenv, s3, subst_type s3 ty)
  | ANDORBinOp (op, exp1, exp2) ->
      let (_, s1, ty1) = ty_exp tyenv exp1 in
      let (_, s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (tyenv, s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (_, s1, ty1) = ty_exp tyenv exp1 in
      let (_, s2, ty2) = ty_exp tyenv exp2 in
      let (_, s3, ty3) = ty_exp tyenv exp3 in
      let eqs4 = (ty1, TyBool) in
      let eqs5 = (ty2, ty3)  in
      let eqs = eqs4 :: (eqs5 :: ((eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3))) in 
      let s6 = unify eqs in (tyenv, s6, subst_type s6 ty3)
  | LetInExp (id, exp1, exp2) ->
      let (_, s1, e1ty) = ty_exp tyenv exp1 in
      let e1sc = closure e1ty tyenv s1 in
      let newtyenv = Environment.extend id e1sc tyenv in
      let (_, s2, e2ty) = ty_exp newtyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (tyenv, s3, subst_type s3 e2ty)
  | LetRecExp (id, para, exp1, exp2) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let domty3 = TyFun (domty1, domty2) in
      let tynewenv = Environment.extend id (TyScheme([], domty3)) (Environment.extend para (TyScheme([], domty1)) tyenv) in
      let (_, s1, ty1) = ty_exp tynewenv exp1 in
      let (_, s2, ty2) = ty_exp tynewenv exp2 in
      let eqs1 = [(domty2, ty1)] in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs1 in
      let s3 = unify eqs in (tyenv, s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      (* let num =
      *   (match domty with
      *     TyVar num -> num
      *    | _ -> err "funexp") in *)
      let (_, s, ranty) = ty_exp (Environment.extend id (TyScheme([], domty)) tyenv) exp in
      (* let e1sc = closure ranty tyenv s in *)
      (tyenv, s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
      let (_, s1, ty1) = ty_exp tyenv exp1 in
      let (_, s2, ty2) = ty_exp tyenv exp2 in
      (match ty1 with
          TyFun(tyy1, tyy2) ->
            let eqs3 = [(tyy1, ty2)] in
            let eqs4 = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
            let s5 = unify eqs4 in (tyenv, s5, subst_type s5 tyy2)
        | TyVar num -> 
            let tyr = TyVar (fresh_tyvar ()) in
            let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(TyVar num, TyFun(ty2, tyr))] in
            let s6 = unify eqs in  (tyenv, s6, subst_type s6 tyr)
        | _ -> pp_ty ty1;err "error AppExp typing")

  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | Decl (id, e) -> 
      let (_, s, ty) = ty_exp tyenv e in
      let esc = closure ty tyenv s in
        (Environment.extend id esc tyenv, s, ty)
  | _ -> err ("Not Implemented!")
