open Eval
open Syntax
open Syntax_debug

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    analysis_exe decl;
      print_newline();
      let (newenv, localenv, value) = eval_decl env decl [] in
        let rec print_localenv tmp_localenv =   
          match tmp_localenv with
              [] -> ()
            | (id, v) :: rest -> pp_id id;pp_val v;print_newline();print_localenv rest
        in print_localenv localenv;
      read_eval_print newenv
  with 
      Lexer.Error -> Printf.printf "lexer error";print_newline();read_eval_print env
    | Eval.Error _ -> print_newline();read_eval_print env
    | _ -> Printf.printf "parser error";print_newline();read_eval_print env

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) 
        (Environment.extend "ii" (IntV 2)
          (Environment.extend "iii" (IntV 3)
            (Environment.extend "iv" (IntV 4) Environment.empty)))))
