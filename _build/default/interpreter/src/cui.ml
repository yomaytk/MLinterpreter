open Eval
open Syntax

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (newenv, localenv, value) = eval_decl env decl [] in
    let rec print_localenv tmp_localenv = 
      match tmp_localenv with
          [] -> ()
        | (id, v) :: rest -> 
            (if except_judge v then
              begin
                pp_id id;pp_val v;print_newline();print_localenv rest;
              end
            else
              begin
                print_localenv rest
              end)
    in print_localenv localenv;
  read_eval_print newenv

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
(Environment.extend "x" (IntV 10) Environment.empty))
