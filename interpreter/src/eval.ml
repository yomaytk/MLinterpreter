open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
  | Except
and dnval = exval

exception Error of string

let err s = raise (Error s)

let andletlist = ref []

let rec andlistadd list tmpenv = 
  let env = tmpenv in
  match list with
      [] -> andletlist := []; env
    | (id, v) :: rest -> andlistadd rest (Environment.extend id v env)

let rec findid list id = 
  match list with
      [] -> true
    | (x, v):: rest ->
        if id = x then false else findid rest id

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"
  | Except -> "error"

let pp_val v = if v!= Except then print_string (string_of_exval v)

let pp_id (i : id) = Printf.printf "val %s = " i

let except_judge v = if v!= Except then true else false

let rec apply_prim op arg1 arg2 = 
  let errm = "Error: Exception Both arguments must be " in
  match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> print_string (errm ^ "integer: +");print_newline();Except
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> print_string (errm ^ "integer: *");print_newline();Except
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> print_string (errm ^ "integer: <");print_newline();Except
  | AMPERAMPER, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | AMPERAMPER, _, _ -> print_string (errm ^ "bool: &&");print_newline();Except
  | PAIPUPAIPU, BoolV i1, BoolV i2 -> BoolV (i1 || i2)
  | PAIPUPAIPU, _, _ -> print_string (errm ^ "bool: ||");print_newline();Except

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
      Environment.Not_bound -> print_string ("Error: Exception Variable not bound: " ^ x);print_newline();Except)
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
      | _ -> print_string "Error: Exception Test expression must be boolean: if";print_newline();Except)
  | LetInExp (id, exp1, exp2) ->
    let value = eval_exp env exp1 in
    let newenv = andlistadd !andletlist env in
    eval_exp (Environment.extend id value newenv) exp2
  | LetAndInExp (id, exp1, exp2) ->
    let bound = findid !andletlist id in
      if bound then Except
      else 
        begin
          let value = eval_exp env exp1 in
          andletlist := (id, value)::!andletlist;eval_exp env exp2
        end
  | FunExp (id, exp) -> ProcV(id, exp, env)
  | AppExp (e1, e2) ->
    let funval = eval_exp env e1 in
    let arg = eval_exp env e2 in
      match funval with
          ProcV(id, exp, env') ->
            let newenv = Environment.extend id arg env' in eval_exp newenv exp
        | _ -> print_string "Error : Non-function value is applied";print_newline();Except


let rec eval_decl env ee (env2 : (Syntax.id * exval) list)=
    match ee with
      Exp e ->
        let v = eval_exp env e in (env, env2 @ [("-", v)], v)
    | Decl (id, e) ->
        let v = eval_exp env e in 
        let newenv = andlistadd !andletlist env in
          if v = Except then (newenv, [("-", v)], v) else (Environment.extend id v newenv, env2 @ [(id, v)], v)
    | RecDecl(id, e1, e2) -> 
        let v = eval_exp env e1 in
          let newenv = Environment.extend id v env in
            eval_decl newenv e2 (env2 @ [(id, v)])
    | AndLet(id, e1, e2) ->
        let bound = findid !andletlist id in
        if bound then (env, env2, Except)
        else
          begin
            let v1 = eval_exp env e1 in
            andletlist := (id, v1) :: !andletlist; eval_decl env e2 (env2 @ [(id, v1)])
          end
    | Rongai -> print_string "Fatal error: Exception Miniml.Parser.MenhirBasics.Error";print_newline();(env, [("-", Except)], Except)
    
