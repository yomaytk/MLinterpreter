
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TYVAR of (
# 9 "tyParser.mly"
       (string)
# 11 "tyParser.ml"
  )
    | RPAREN
    | LPAREN
    | LIST
    | INT
    | EOF
    | BOOL
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState7
  | MenhirState2
  | MenhirState0

# 1 "tyParser.mly"
  
open TySyntax

# 44 "tyParser.ml"

let rec _menhir_goto_TFun : _menhir_env -> 'ttv_tail -> _menhir_state -> (TySyntax.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (TySyntax.ty))), _, (t2 : (TySyntax.ty))) = _menhir_stack in
        let _2 = () in
        let _v : (TySyntax.ty) = 
# 20 "tyParser.mly"
                          ( TyFun (t1, t2) )
# 58 "tyParser.ml"
         in
        _menhir_goto_TFun _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (TySyntax.ty))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (TySyntax.ty) = 
# 26 "tyParser.mly"
                         ( t )
# 76 "tyParser.ml"
             in
            _menhir_goto_TLit _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (TySyntax.ty))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 12 "tyParser.mly"
      (TySyntax.ty)
# 98 "tyParser.ml"
            ) = 
# 16 "tyParser.mly"
               ( t )
# 102 "tyParser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 12 "tyParser.mly"
      (TySyntax.ty)
# 109 "tyParser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_TLit : _menhir_env -> 'ttv_tail -> _menhir_state -> (TySyntax.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARROW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | LPAREN ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | TYVAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | LIST ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (t : (TySyntax.ty))) = _menhir_stack in
        let _2 = () in
        let _v : (TySyntax.ty) = 
# 27 "tyParser.mly"
                ( TyList t )
# 152 "tyParser.ml"
         in
        _menhir_goto_TLit _menhir_env _menhir_stack _menhir_s _v
    | EOF | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (t : (TySyntax.ty))) = _menhir_stack in
        let _v : (TySyntax.ty) = 
# 19 "tyParser.mly"
           ( t )
# 161 "tyParser.ml"
         in
        _menhir_goto_TFun _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "tyParser.mly"
       (string)
# 189 "tyParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (name : (
# 9 "tyParser.mly"
       (string)
# 197 "tyParser.ml"
    )) = _v in
    let _v : (TySyntax.ty) = 
# 25 "tyParser.mly"
               ( TyVar name )
# 202 "tyParser.ml"
     in
    _menhir_goto_TLit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TYVAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (TySyntax.ty) = 
# 23 "tyParser.mly"
        ( TyInt )
# 233 "tyParser.ml"
     in
    _menhir_goto_TLit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (TySyntax.ty) = 
# 24 "tyParser.mly"
         ( TyBool )
# 245 "tyParser.ml"
     in
    _menhir_goto_TLit _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and toplevel : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 12 "tyParser.mly"
      (TySyntax.ty)
# 264 "tyParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TYVAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/home/masashi/.opam/4.07.1/lib/menhir/standard.mly"
  

# 294 "tyParser.ml"
