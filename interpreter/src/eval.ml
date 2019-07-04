open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp * dnval Environment.t ref
  | ConsV of exval * exval
  | NilV
  | Exception
and dnval = exval

exception Error of string

let err s = raise (Error s)

let andletlist = ref []

(*リストで環境を拡張し、拡張された環境を返す関数*)
let rec andlistadd list tmpenv = 
  let env = tmpenv in
  match list with
      [] -> andletlist := []; env
    | (id, v) :: rest -> andlistadd rest (Environment.extend id v env)

(*リストの中に id と同じ名前が存在する場合 true そうでない時 false を返すような関数*)
let rec findid list id = 
  match list with
      [] -> false
    | (x, _):: rest ->
        if id = x then true else findid rest id

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"
  | DProcV _ -> "<fun>"
  | ConsV (e1, e2) -> "ConsV(" ^ (string_of_exval e1) ^ ", " ^ (string_of_exval e2) ^ ")"
  | NilV -> "NilV"
  | Exception -> "error"

let string_of_binop = function
    Plus -> "Plus"
  | Mult -> "Mult"
  | Lt -> "Lt"
  | AAND -> "AAND"
  | OOR -> "OOR"

(*exval型の値を受け取ってそれを文字列として出力する関数*)
let pp_val v = print_string (string_of_exval v)

let pp_id (i : id) = Printf.printf "val %s = " i

let rec apply_prim op arg1 arg2 = 
  let errm = "Error: Exception Both arguments must be " in
  match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> print_string (errm ^ "integer: +");err "error"
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> print_string (errm ^ "integer: *");err "error"
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> print_string (errm ^ "integer: <");err "error"
  | AAND, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | AAND, _, _ -> print_string (errm ^ "bool: &&");err "error"
  | OOR, BoolV i1, BoolV i2 -> BoolV (i1 || i2)
  | OOR, _, _ -> print_string (errm ^ "bool: ||");err "error"

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
        Environment.Not_bound -> (*print_string ("Error: Exception Variable not bound: " ^ x);*)err "error")
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | NIlV -> NilV
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | ANDORBinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    (match op with
        (*false && のときは即 false に決定、そうでない時は全体を再評価*)
        AAND -> if arg1 = BoolV false then BoolV false else eval_exp env (BinOp(op, exp1, exp2))
        (*true || の時は即 true に決定、そうでない時は全体を再評価*)
        | OOR -> if arg1 = BoolV true then BoolV true else eval_exp env (BinOp(op, exp1, exp2))
        | _ -> err "error")
  | FplmuBinOp (op, id1, id2) ->
      let exp1 = try Environment.lookup id1 env with _ -> err "error" in
      let exp2 = try Environment.lookup id2 env with _ -> err "error" in
      apply_prim op exp1 exp2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
        BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> print_string "Error: Exception Test expression must be boolean: if";err "error")
  | LetInExp (id, exp1, exp2) ->
    let value = eval_exp env exp1 in
    let newenv = andlistadd !andletlist env in
    eval_exp (Environment.extend id value newenv) exp2
  | LetAndInExp (id, exp1, exp2) ->
      (*新たに束縛しようとしている変数と同じ名前の変数がすでに宣言されていないか findid で判定*)
    let bound = findid !andletlist id in
    if bound then err "error"(*すでに同じ名前の変数が存在する場合はエラー*)
      else begin
        let value = eval_exp env exp1 in
        (*リストandletlistに新しい変数を追加*)
          andletlist := (id, value)::!andletlist;eval_exp env exp2
        end
  | LetEndInExp (id, exp1, exp2) ->
      let bound = findid !andletlist id in
      if bound then err "error"
      else begin
          let value = eval_exp env exp1 in
          (*リストに追加された変数を一斉に環境に追加*)
          let newenv = andlistadd !andletlist env in
          eval_exp (Environment.extend id value newenv) exp2
        end
  | FunExp (id, exp) -> ProcV(id, exp, ref env)
  | DfunExp (id, exp) -> DProcV(id, exp, ref env)
  | FplmuFunExp(op, exp, id2) ->
      (match id2 with
        (*引数が０個の場合、もしくは関数適用で左結合される場合の処理*)
        "--"  -> ProcV("a", FunExp("b", FplmuBinOp(op, "a", "b")),ref env)
      (*引数が１個の場合の処理*)
      |  _ -> (let arga = eval_exp env exp in
                ProcV("b", FplmuBinOp(op, "a", "b"), ref (Environment.extend "a" arga env))))
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
        (match funval with
          ProcV (id, body, env') ->
            let newenv = Environment.extend id arg !env' in  (*ProcVから取り出した環境を拡張*)
            let newenv2 = Environment.extend id arg env in   (*今現在の環境を拡張*)
            (*相互再帰関数の時のために、失敗した場合現在の環境で再評価*)
            (try eval_exp newenv body with _ -> eval_exp newenv2 body)
        | DProcV (id, body, env') ->
            let newenv = Environment.extend id arg !env' in
            let newenv2 = Environment.extend id arg env in
            (*動的束縛の時は、今現在の環境で最初に評価*)
            (try eval_exp newenv2 body with _ -> eval_exp newenv body)
        | _ -> print_string "Error : Non-function value is applied";err "error")
  | LetRecExp (id, para, exp1, exp2) ->
      (*ダミーの環境の参照を用意*)
      let dummyenv = ref Environment.empty in
      (* 関数閉包を作り，id をこの関数閉包に写像するように現在の環境env を拡張*)
      let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
      (*ダミーの環境に新しい環境を破壊的代入*)
      dummyenv := newenv;
      eval_exp newenv exp2
  | ListExp (e1, e2) ->
      let el = eval_exp env e1 in
      ConsV (el, (eval_exp env e2)) 

let rec eval_decl env ee (env2 : (Syntax.id * exval) list) =
    match ee with
      Exp e ->
        let v = eval_exp env e in (env, env2 @ [("-", v)], v)
    | Decl (id, e) ->
        let v = eval_exp env e in
        let bound = findid !andletlist id in
        if bound then begin Printf.printf "Error: Variable a is bound several times in this matching"; (env, [], err "error" ) end
        else begin
                let newenv = andlistadd !andletlist env in
                if v = Exception then (newenv, [("-", v)], v) else (Environment.extend id v newenv, env2 @ [(id, v)], v)
              end
    | DecDecl(id, e1, e2) ->
        let v = eval_exp env e1 in
          let newenv = Environment.extend id v env in
            eval_decl newenv e2 (env2 @ [(id, v)])
    | AndLet(id, e1, e2) ->
        let bound = findid !andletlist id in
        if bound then begin Printf.printf "Error: Variable a is bound several times in this matching";  (env, [], err "error") end
        else begin
            let v1 = eval_exp env e1 in
            andletlist := (id, v1) :: !andletlist; eval_decl env e2 (env2 @ [(id, v1)])
          end
    | RecDecl (id1, id2, e) ->
        let dummyenv = ref Environment.empty in
          let newenv = Environment.extend id1 (ProcV(id2, e, dummyenv)) env in
          dummyenv := newenv;(newenv, env2 @ [(id1, ProcV(id2, e, dummyenv))], ProcV(id2, e, dummyenv))
    | RecAndLet (id1, id2, e1, e2) ->
        let dummyenv = ref Environment.empty in
        let newenv = Environment.extend id1 (ProcV(id2, e1, dummyenv)) env in
        dummyenv := newenv;eval_decl newenv e2 (env2 @ [(id1, ProcV(id2, e1, dummyenv))])
    | ParseFail -> print_string "Error: Exception Miniml.Parser.MenhirBasics.Error";(env, [("-", Exception)], err "error")
    
