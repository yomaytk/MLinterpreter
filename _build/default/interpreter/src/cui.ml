open Eval
open Syntax

let rec print_val env = 
  match env with
      [] -> ()
    | (id, v)::rest -> 
      if v != Except then 
        begin
          Printf.printf "val %s = " id;
          pp_val v;
          print_newline();
          print_val rest
        end

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let localenv = Environment.extend "-" Except Environment.empty in
  let (newenv, env2) = eval_decl env decl localenv in
    print_val env2;
  (* if v != Except then   
    (Printf.printf "val %s = " id;pp_val v;print_newline()); *)
  read_eval_print newenv

let a = Environment.extend "x" (IntV 10) Environment.empty

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) Environment.empty))
