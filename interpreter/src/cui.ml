open Eval
open Syntax

let rec read_eval_print env envv =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (newenv, env2) = eval_decl env decl envv in
  (* if v != Except then   
    (Printf.printf "val %s = " id;pp_val v;print_newline()); *)
  read_eval_print newenv envv

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) Environment.empty))
