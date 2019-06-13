open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | Except
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | Except -> "except"

let pp_val v = if v!= Except then print_string (string_of_exval v)

let pp_id (i : id) = Printf.printf "val %s = " i

let except_judge v = if v!= Except then true else false

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> print_string "Fatal error: Exception Both arguments must be integer: +";print_newline();Except
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> print_string "Fatal error: Exception Both arguments must be integer: *";print_newline();Except
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> print_string "Fatal error: Exception Both arguments must be integer: <";print_newline();Except
  | AMPERAMPER, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | AMPERAMPER, _, _ -> print_string "Fatal error: Exception Both arguments must be bool: &&";print_newline();Except
  | PAIPUPAIPU, BoolV i1, BoolV i2 -> BoolV (i1 || i2)
  | PAIPUPAIPU, _, _ -> print_string "Fatal error: Exception Both arguments must be bool: ||";print_newline();Except

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
      Environment.Not_bound -> print_string ("Fatal error: Exception Variable not bound: " ^ x);print_newline();Except)
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
        BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> print_string "Fatal error: Exception Test expression must be boolean: if";print_newline();Except)
  | LetInExp (id, exp1, exp2) ->
    let value = eval_exp env exp1 in
    eval_exp (Environment.extend id value env) exp2


let rec eval_decl env ee (env2 : (Syntax.id * exval) list)=
    match ee with
      Exp e ->
        let v = eval_exp env e in (env, env2 @ [("-", v)])
    | Decl (id, e) ->
        let v = eval_exp env e in 
          if v = Except then (env, [("-", v)]) else (Environment.extend id v env, env2 @ [(id, v)])
    | DeclDecl(id, e1, e2) -> 
        let v = eval_exp env e1 in
          let newenv = Environment.extend id v env in
            eval_decl newenv e2 (env2 @ [(id, v)])
    | Rongai -> print_string "Fatal error: Exception Miniml.Parser.MenhirBasics.Error";print_newline();(env, [("-", Except)])
    