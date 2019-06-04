open Eval
open Syntax

(* let rec print_val env =
  let _ = Environment.extend "-" Except env in
  match env with
      _ -> ()
    | (x, v)::rest -> () *)

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let env2 = Environment.empty in
  let (newenv, localenv) = eval_decl env decl env2 in
  (* if v != Except then   
    (Printf.printf "val %s = " id;pp_val v;print_newline()); *)
  Environment.print_env pp_val localenv;
  read_eval_print newenv

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) Environment.empty))
