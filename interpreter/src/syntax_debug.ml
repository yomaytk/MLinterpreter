open Syntax
open Eval

let rec exp_analysis exp = 
  match exp with
    Var x -> "Var " ^ x 
  | ILit i -> "ILit " ^ string_of_int i
  | BLit b -> "BLit " ^ string_of_bool b
  | NIlV -> "NilV"
  | BinOp (op, exp1, exp2) -> "BinOp(" ^ (string_of_binop op) ^ ", " ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")"
  | ANDORBinOp (op, exp1,exp2) -> "ANDORBinOp(" ^ (string_of_binop op) ^ ", " ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")"
  | FplmuBinOp (op, id1, id2) -> "FplmuBinOp(" ^ (string_of_binop op) ^ ", " ^ id1 ^ ", " ^ id2 ^ ")"
  | IfExp (exp1, exp2, exp3) -> "IfExp(" ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ", " ^ (exp_analysis exp3) ^ ")"
  | LetInExp (id, exp1, exp2) -> "LetInExp(" ^ id ^ ", " ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")" 
  | LetAndInExp (id, exp1, exp2) -> "LetAndInExp(" ^ id ^ ", " ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")"
  | LetEndInExp (id, exp1, exp2) -> "LetEndInExp(" ^ id ^ ", " ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")"
  | FunExp (id, exp) -> "FunExp(" ^ id ^ ", " ^ (exp_analysis exp) ^ ")"
  | DfunExp (id, exp) -> "DfunExp(" ^ id ^ ", " ^ (exp_analysis exp) ^ ")"
  | FplmuFunExp (op, exp, id) -> "FplmuFunExp(" ^ (string_of_binop op) ^ ", " ^ (exp_analysis exp) ^ ", " ^ id ^ ")"
  | AppExp (exp1, exp2) -> "AppExp(" ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")"
  | LetRecExp (id1, id2, exp1, exp2) -> "LetRecExp(" ^ id1 ^ ", " ^ id2 ^ ", " ^ (exp_analysis exp1) ^ ", " ^ (exp_analysis exp2) ^ ")"
  | ListExp (e1, e2) -> "ListExp(" ^ (exp_analysis e1) ^ ", " ^ (exp_analysis e2) ^ ")"
  | ListFirstExp (e) -> "ListFirstExp(" ^ (exp_analysis e) ^ ")"
  | _ -> "Fail!"

let rec program_analysis decl = 
  match decl with
    Exp e -> "Exp(" ^ (exp_analysis e) ^ ")"
  | DecDecl (id, e1, e2) -> "DecDecl(" ^ id ^ ", " ^ (exp_analysis e1) ^ ", " ^ (program_analysis e2) ^ ")"
  | Decl (id, e) -> "Decl(" ^ id ^ ", " ^ exp_analysis e ^ ")"
  | AndLet (id, e1, e2) -> "AndLet(" ^ id ^ ", " ^ (exp_analysis e1) ^ ", " ^ (program_analysis e2) ^ ")"
  | RecDecl (id1, id2, e) -> "RecDecl(" ^ id1 ^ ", " ^ id2 ^ ", " ^ (exp_analysis e) ^ ")"
  | RecAndLet (id1, id2, e1, e2) -> "RecAndLet(" ^ id1 ^ ", " ^ id2 ^ ", " ^ (exp_analysis e1) ^ ", " ^ (program_analysis e2) ^ ")" 
  | ParseFail -> "ParseFail"
  | _ -> "Fail!"

let analysis_exe decl = 
  let s = program_analysis decl in
  print_string s
