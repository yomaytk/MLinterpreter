{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("fun", Parser.FUN);
  ("and", Parser.AND);
  ("rec", Parser.REC);
  ("dfun", Parser.DFUN);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
];;
let cnt = ref 0
exception Error
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(*" { cnt := 1;comment lexbuf }
| "*)" { raise Error }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "&&" { Parser.AAND }
| "||" { Parser.OOR }
| "->" { Parser.RARROW }
| "(+)" { Parser.FPLUS }
| "( * )" { Parser.FMULT }
| "[" { Parser.MDRPAREN }
| "]" { Parser.MDLPAREN }
| ";" { Parser.SEMI }
| "::" { Parser.COROCORO }
| "|" { Parser.PAIPU }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }

| _ { raise Error }

| eof { exit 0 }

and comment = parse
      (*cntをインクリメントする*)
    "(*" {cnt := !cnt+1;comment lexbuf}
      (*cntをデクリメントし、cntの値が正、０、負のそれぞれについて処理を行う*)           
  | "*)" {cnt := !cnt-1;if !cnt = 0 then main lexbuf else if !cnt > 0 then comment lexbuf else raise Error }
  (*途中の文の時はそのままコメントの規則を再帰的に呼び出す*)
  | _ {comment lexbuf}

  | eof { exit 0 }


