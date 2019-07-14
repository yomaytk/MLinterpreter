open Syntax
exception Error of string

let err s = raise (Error s)

let err2 () = raise (Environment.Not_bound)
(* Type Environment *)
type tyenv = tysc Environment.t

type subst = (tyvar * ty) list

let idenv = ref Environment.empty

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
	| TyList ty1 -> (research_ftv num ty1)
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
				| TyList ty1, TyList ty2 -> unify ((ty1, ty2) :: rest)
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

let unionScheme (TyScheme(tysc1, _)) (TyScheme(tysc2, _)) = tysc1 @ tysc2

let rec assigntyvar tyenv idl = 
	match idl with
			[] -> tyenv
		| id :: rest -> assigntyvar (Environment.extend id (TyScheme([], TyVar (fresh_tyvar ()))) tyenv) rest

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
				with Environment.Not_bound -> err2 ())
	| ILit _ -> (tyenv, [], TyInt)
	| BLit _ -> (tyenv, [], TyBool)
	| NIlV ->  (tyenv, [], TyList (TyVar (fresh_tyvar ())))
	| BinOp (op, exp1, exp2) ->
			let (_, s1, ty1) = ty_exp tyenv exp1 in
			let (_, s2, ty2) = ty_exp tyenv exp2 in
			let (eqs3, ty) = ty_prim op ty1 ty2 in
			let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
			let s3 = unify eqs in (tyenv, s3, subst_type s3 ty)
	| ANDORBinOp (op, exp1, _) ->
			let (_, s1, ty1) = ty_exp tyenv exp1 in
			(* let (_, s2, ty2) = ty_exp tyenv exp2 in *)
			let (eqs3, ty) = ty_prim op ty1 ty1 in
			let eqs = (eqs_of_subst s1) @  eqs3 in
			let s3 = unify eqs in (tyenv, s3, subst_type s3 ty)
	| FplmuBinOp (op, id1, id2) ->
			let e1 = Var id1 in
			let e2 = Var id2 in
			let (_, s1, ty1) = ty_exp tyenv e1 in
			let (_, s2, ty2) = ty_exp tyenv e2 in
			let (eqs1, ty) = ty_prim op ty1 ty2 in
			let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs1 in
			let s3 = unify eqs in (tyenv, s3, subst_type s3 ty)
	| FplmuFunExp(_, exp, id) -> 
			(match id with
					"--" -> 
							(tyenv, [], TyFun (TyInt, (TyFun (TyInt, TyInt))))
				| _ ->  let (_, s1, ty1) = ty_exp tyenv exp in
								let eqs1 = [(ty1, TyInt)] in
								let eqs = (eqs_of_subst s1) @ eqs1 in
								let s2 = unify eqs in (tyenv, s2, TyFun (ty1, (TyFun(TyInt, TyInt)))))
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
	| LetAndInExp (id, exp1, exp2) -> 
			let (_, s1, e1ty) = ty_exp tyenv exp1 in
			let e1sc = closure e1ty tyenv s1 in
			let newtyenv = Environment.extend id e1sc tyenv in
			ty_exp newtyenv exp2
	| LetEndInExp (id, exp1, exp2) -> 
			let (_, s1, e1ty) = ty_exp tyenv exp1 in
			let e1sc = closure e1ty tyenv s1 in
			let newtyenv = Environment.extend id e1sc tyenv in
			let (_, s2, e2ty) = ty_exp newtyenv exp2 in
			let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
			let s3 = unify eqs in (tyenv, s3, subst_type s3 e2ty)
	| LetRecExp (id, para, exp1, exp2) ->
			let domty1 = TyVar (fresh_tyvar ()) in
			let domty2 = TyVar (fresh_tyvar ()) in
			let tynewenv = Environment.extend id (TyScheme([], TyFun(domty1, domty2))) (Environment.extend para (TyScheme([], domty1)) tyenv) in
			let (_, s1, ty2) = ty_exp tynewenv exp1 in
			let eqs1 = [(domty2, ty2)] in
			let eqs2 = (eqs_of_subst s1) @ eqs1 in
			let su = unify eqs2 in
			let ty1 = subst_type su domty1 in
			let ty2 = subst_type su domty2 in
			let e1sc = closure ty1 tyenv su in
			let e2sc = closure ty2 tyenv su in
			let tynewenv2 = Environment.extend id (TyScheme(unionScheme e1sc e2sc, TyFun(ty1, ty2))) tyenv in
			let (_, s2, ty3) = ty_exp tynewenv2 exp2 in
			let eqs = eqs2 @ (eqs_of_subst s2) in
			let s3 = unify eqs in (tyenv, s3, subst_type s3 ty3)
	| FunExp (id, exp) ->
			let domty = TyVar (fresh_tyvar ()) in
			let (_, s1, ranty) = ty_exp (Environment.extend id (TyScheme([], domty)) tyenv) exp in
			(tyenv, s1, TyFun (subst_type s1 domty, ranty))
	| DfunExp (id, exp) -> 
			let domty = TyVar (fresh_tyvar ()) in
			let (_, s1, ranty) = ty_exp (Environment.extend id (TyScheme([], domty)) tyenv) exp in
			(tyenv, s1, TyFun (subst_type s1 domty, ranty))
	| AppExp (exp1, exp2) -> 
			let (_, s1, ty1) = 
				try ty_exp tyenv exp1 with Environment.Not_bound -> 
					let tyenv = assigntyvar tyenv (Eval.getVar exp1) in ty_exp tyenv exp1 
			in
			let (_, s2, ty2) = 
				try ty_exp tyenv exp2 with Environment.Not_bound -> 
					let tyenv = assigntyvar tyenv (Eval.getVar exp2) in ty_exp tyenv exp2 
			in
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
	| ListExp (e1, e2) -> 
			let (_, s1, ty1) = ty_exp tyenv e1 in
			let (_, s2, ty2) = ty_exp tyenv e2 in
				(match ty2 with
						TyList (TyVar num) -> 
						print_string "ajijij";
								let eqs1 = [(TyVar num, ty1)] in
								let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs1 in
								let s3 = unify eqs in (tyenv, s3, subst_type s3 (TyList ty1))
					| _ -> 
					pp_ty ty1;pp_ty ty2;
								let eqs1 = [(TyList ty1, ty2)] in
								let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs1 in
								let s3 = unify eqs in (tyenv, s3, subst_type s3 (TyList ty1)))
	| MatchExp (e1, e2, id1, id2, e3) -> 
			let (_, s1, ty1) = ty_exp tyenv e1 in
			let (_, s2, ty2) = ty_exp tyenv e2 in
					(match ty1 with
								TyList ty ->
										let tynewenv = Environment.extend id1 (TyScheme ([], ty)) (Environment.extend id2 (TyScheme([], TyList ty)) tyenv) in
										let (_, s3, ty3) = ty_exp tynewenv e3 in
										let eqs1 = [(ty2, ty3)] in
										let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ eqs1 in
										let s4 = unify eqs in (tyenv, s4, subst_type s4 (TyList ty2))
							| _ -> err "matchexp error")
	(* | _ -> err ("Not Implemented!") *)

let rec ty_decl tyenv = function
		Exp e -> ty_exp tyenv e
	| Decl (id, e) -> 
			let (_, s, ty) = ty_exp tyenv e in
			let esc = closure ty tyenv s in
				idenv := Environment.extend id ty !idenv;((Environment.extend id esc tyenv), s, ty)
	| DecDecl (id, e1, e2) ->
			let (_, s, ty) = ty_exp tyenv e1 in
			let esc = closure ty tyenv s in
				idenv := Environment.extend id ty !idenv;ty_decl (Environment.extend id esc tyenv) e2
	| AndLet (id, e1, e2) ->
			let (_, s, ty) = ty_exp tyenv e1 in
			let esc = closure ty tyenv s in
				idenv := Environment.extend id ty !idenv;ty_decl (Environment.extend id esc tyenv) e2
	| RecDecl (id, para, e) ->
			let domty1 = TyVar (fresh_tyvar ()) in
			let domty2 = TyVar (fresh_tyvar ()) in
			let tynewenv = Environment.extend id (TyScheme([], TyFun(domty1, domty2))) (Environment.extend para (TyScheme([], domty1)) tyenv) in
			let (_, s1, tye2) = ty_exp tynewenv e in
			let eqs1 = [(tye2, domty2)] in
			let eqs = (eqs_of_subst s1) @ eqs1 in
			let s2 = unify eqs in
			let tye1 = subst_type s2 domty1 in
			let tye2 = subst_type s2 domty2 in
			let esc1 = closure tye1 tyenv s2 in
			let esc2 = closure tye2 tyenv s2 in
			idenv := Environment.extend id (TyFun(tye1, tye2)) !idenv;(Environment.extend id (TyScheme(unionScheme esc1 esc2, TyFun (tye1, tye2))) tyenv, s2, TyFun(tye1, tye2))
	| RecAndLet (id, para, e, e2) ->
			let domty1 = TyVar (fresh_tyvar ()) in
			let domty2 = TyVar (fresh_tyvar ()) in
			let tynewenv = Environment.extend id (TyScheme([], TyFun(domty1, domty2))) (Environment.extend para (TyScheme([], domty1)) tyenv) in
			let (_, s1, tye2) = ty_exp tynewenv e in
			let eqs1 = [(tye2, domty2)] in
			let eqs = (eqs_of_subst s1) @ eqs1 in
			let s2 = unify eqs in
			let tye1 = subst_type s2 domty1 in
			let tye2 = subst_type s2 domty2 in
			let esc1 = closure tye1 tyenv s2 in
			let esc2 = closure tye2 tyenv s2 in
			idenv := Environment.extend id (TyFun(tye1, tye2)) !idenv;ty_decl (Environment.extend id (TyScheme(unionScheme esc1 esc2, TyFun (tye1, tye2))) tyenv) e2
