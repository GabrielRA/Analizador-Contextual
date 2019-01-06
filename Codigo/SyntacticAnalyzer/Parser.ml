type token =
  | INTLITERAL of (string)
  | CHARLITERAL of (string)
  | IDENTIFIER of (string)
  | OPERATOR of (string)
  | ARRAY
  | BEGIN
  | CONST
  | DO
  | ELSE
  | END
  | FUNC
  | IF
  | IN
  | LET
  | OF
  | PROC
  | RECORD
  | THEN
  | TYPE
  | VAR
  | WHILE
  | DOT
  | COLON
  | SEMICOLON
  | COMMA
  | BECOMES
  | IS
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LCURLY
  | RCURLY
  | EOF

open Parsing;;
# 2 "Parser.mly"

(* --------------------------------------------------- *)
(* Parser generator file (ocamlyacc) for Caml-Triangle *)
(*                                                     *)
(* (c) 2006 Luis Leopoldo Pérez.                       *)
(* Last modification: April 12, 2006                   *)
(* --------------------------------------------------- *)

      open Ast
      open Printf
      open Parsing
      open ErrorReporter
      open RuntimeEntity
      
      let parse_error s = ()
# 55 "Parser.ml"
let yytransl_const = [|
  261 (* ARRAY *);
  262 (* BEGIN *);
  263 (* CONST *);
  264 (* DO *);
  265 (* ELSE *);
  266 (* END *);
  267 (* FUNC *);
  268 (* IF *);
  269 (* IN *);
  270 (* LET *);
  271 (* OF *);
  272 (* PROC *);
  273 (* RECORD *);
  274 (* THEN *);
  275 (* TYPE *);
  276 (* VAR *);
  277 (* WHILE *);
  278 (* DOT *);
  279 (* COLON *);
  280 (* SEMICOLON *);
  281 (* COMMA *);
  282 (* BECOMES *);
  283 (* IS *);
  284 (* LPAREN *);
  285 (* RPAREN *);
  286 (* LBRACKET *);
  287 (* RBRACKET *);
  288 (* LCURLY *);
  289 (* RCURLY *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INTLITERAL *);
  258 (* CHARLITERAL *);
  259 (* IDENTIFIER *);
  260 (* OPERATOR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\014\000\014\000\015\000\015\000\004\000\004\000\004\000\
\008\000\008\000\008\000\016\000\016\000\016\000\016\000\016\000\
\018\000\018\000\019\000\019\000\020\000\020\000\020\000\020\000\
\007\000\007\000\021\000\021\000\022\000\022\000\022\000\022\000\
\017\000\017\000\017\000\023\000\023\000\012\000\013\000\006\000\
\011\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\000\000\003\000\004\000\003\000\
\004\000\006\000\004\000\001\000\004\000\006\000\001\000\001\000\
\003\000\001\000\001\000\001\000\004\000\002\000\003\000\003\000\
\003\000\003\000\005\000\001\000\003\000\001\000\003\000\004\000\
\001\000\003\000\001\000\004\000\004\000\007\000\009\000\004\000\
\000\000\001\000\001\000\003\000\003\000\004\000\005\000\007\000\
\000\000\001\000\001\000\003\000\001\000\002\000\002\000\002\000\
\001\000\004\000\003\000\003\000\005\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\064\000\000\000\000\000\000\000\000\000\
\066\000\000\000\000\000\000\000\000\000\000\000\015\000\062\000\
\063\000\065\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\018\000\019\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\031\000\006\000\000\000\000\000\000\000\
\000\000\053\000\000\000\050\000\000\000\000\000\000\000\023\000\
\000\000\025\000\000\000\024\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\009\000\034\000\011\000\032\000\
\056\000\055\000\000\000\030\000\007\000\000\000\000\000\013\000\
\029\000\000\000\000\000\021\000\036\000\000\000\000\000\000\000\
\000\000\000\000\042\000\000\000\000\000\000\000\000\000\057\000\
\040\000\037\000\052\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\027\000\000\000\000\000\000\000\045\000\000\000\044\000\
\000\000\000\000\000\000\059\000\000\000\000\000\046\000\000\000\
\038\000\058\000\000\000\000\000\047\000\000\000\000\000\000\000\
\039\000\061\000\048\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\024\000\074\000\026\000\075\000\038\000\
\027\000\028\000\029\000\030\000\031\000\054\000\052\000\039\000\
\121\000\114\000\115\000\116\000\076\000\077\000\136\000"

let yysindex = "\010\000\
\012\255\000\000\000\000\000\000\122\255\180\255\198\255\180\255\
\000\000\040\000\022\255\080\255\026\255\043\255\000\000\000\000\
\000\000\000\000\180\255\198\255\180\255\180\255\057\255\006\255\
\044\255\040\255\072\255\000\000\081\255\000\000\000\000\000\000\
\057\255\057\255\057\255\057\255\057\255\249\254\000\000\069\255\
\000\000\122\255\057\255\180\255\180\255\158\255\000\000\071\255\
\003\255\059\255\068\255\064\255\076\255\074\255\122\255\158\255\
\081\255\000\000\078\255\084\255\086\255\089\255\094\255\122\255\
\184\255\122\255\000\000\000\000\000\000\091\255\057\255\057\255\
\057\255\000\000\095\255\000\000\098\255\180\255\180\255\000\000\
\180\255\000\000\180\255\000\000\117\255\100\255\000\000\180\255\
\036\255\036\255\070\255\070\255\000\000\000\000\000\000\000\000\
\000\000\000\000\006\255\000\000\000\000\158\255\126\255\000\000\
\000\000\106\255\122\255\000\000\000\000\057\255\057\255\057\255\
\118\255\115\255\000\000\127\255\116\255\155\255\057\255\000\000\
\000\000\000\000\000\000\180\255\057\255\000\000\129\255\135\255\
\141\255\070\255\142\255\036\255\139\255\160\255\153\255\167\255\
\000\000\000\000\036\255\036\255\070\255\000\000\070\255\000\000\
\122\255\070\255\070\255\000\000\150\255\156\255\000\000\162\255\
\000\000\000\000\168\255\164\255\000\000\180\255\057\255\070\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\004\000\000\000\000\000\000\000\254\254\000\000\000\000\000\000\
\000\000\000\000\007\000\000\000\120\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\001\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\170\255\000\000\000\000\
\000\000\000\000\165\255\000\000\000\000\000\000\188\255\170\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\172\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\173\255\173\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\061\255\000\000\000\000\000\000\000\000\000\000\
\000\000\182\255\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\177\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\173\255\173\255\000\000\000\000\000\000\000\000\
\025\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\197\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255\215\255\066\000\013\000\008\000\157\000\196\000\
\000\000\229\255\192\000\102\000\000\000\096\000\141\000\158\000\
\174\255\186\255\092\000\000\000\123\000\000\000\067\000"

let yytablesize = 344
let yytable = "\014\000\
\030\000\058\000\005\000\005\000\005\000\064\000\003\000\005\000\
\013\000\122\000\001\000\003\000\013\000\085\000\004\000\079\000\
\065\000\005\000\025\000\117\000\040\000\005\000\093\000\006\000\
\095\000\007\000\065\000\043\000\020\000\087\000\053\000\048\000\
\008\000\050\000\051\000\045\000\067\000\005\000\004\000\041\000\
\059\000\060\000\061\000\062\000\063\000\042\000\110\000\142\000\
\005\000\013\000\068\000\111\000\047\000\046\000\012\000\112\000\
\069\000\070\000\151\000\004\000\152\000\055\000\013\000\154\000\
\155\000\126\000\012\000\056\000\149\000\150\000\012\000\013\000\
\004\000\013\000\118\000\018\000\066\000\163\000\097\000\098\000\
\100\000\016\000\017\000\004\000\018\000\054\000\119\000\080\000\
\078\000\054\000\103\000\104\000\081\000\051\000\082\000\106\000\
\113\000\113\000\120\000\120\000\109\000\043\000\083\000\153\000\
\088\000\044\000\084\000\012\000\021\000\045\000\022\000\089\000\
\023\000\090\000\013\000\091\000\092\000\127\000\128\000\129\000\
\012\000\096\000\102\000\101\000\004\000\107\000\135\000\005\000\
\108\000\012\000\125\000\012\000\053\000\006\000\124\000\007\000\
\137\000\120\000\099\000\113\000\130\000\030\000\008\000\131\000\
\133\000\030\000\113\000\113\000\120\000\030\000\120\000\132\000\
\013\000\120\000\120\000\016\000\139\000\015\000\016\000\017\000\
\004\000\018\000\140\000\141\000\143\000\145\000\135\000\120\000\
\071\000\019\000\161\000\020\000\012\000\072\000\146\000\147\000\
\148\000\073\000\156\000\015\000\016\000\017\000\004\000\018\000\
\157\000\021\000\160\000\022\000\158\000\023\000\033\000\019\000\
\159\000\020\000\034\000\028\000\005\000\032\000\049\000\035\000\
\051\000\041\000\036\000\037\000\033\000\043\000\060\000\021\000\
\034\000\022\000\012\000\023\000\086\000\035\000\026\000\049\000\
\036\000\037\000\057\000\134\000\138\000\105\000\094\000\144\000\
\123\000\162\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\030\000\030\000\030\000\005\000\005\000\030\000\005\000\005\000\
\003\000\000\000\030\000\000\000\000\000\000\000\030\000\000\000\
\030\000\030\000\005\000\005\000\005\000\030\000\030\000\030\000\
\020\000\030\000\000\000\000\000\020\000\020\000\020\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\000\000\000\000\020\000\020\000\000\000\000\000\
\000\000\020\000\000\000\020\000\000\000\020\000\012\000\012\000\
\012\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\012\000\012\000\
\000\000\000\000\000\000\012\000\000\000\012\000\000\000\012\000"

let yycheck = "\005\000\
\000\000\029\000\000\000\000\000\000\000\013\001\000\000\010\001\
\001\000\092\000\001\000\000\001\005\000\055\000\003\001\013\001\
\024\001\006\001\006\000\090\000\008\000\024\001\064\000\012\001\
\066\000\014\001\024\001\022\001\000\000\057\000\023\000\019\000\
\021\001\021\000\022\000\030\001\042\000\013\001\003\001\000\000\
\033\000\034\000\035\000\036\000\037\000\024\001\011\001\130\000\
\024\001\042\000\043\000\016\001\010\001\028\001\000\000\020\001\
\044\000\045\000\141\000\003\001\143\000\018\001\055\000\146\000\
\147\000\107\000\001\000\028\001\139\000\140\000\005\000\064\000\
\003\001\066\000\005\001\004\001\008\001\160\000\071\000\072\000\
\073\000\001\001\002\001\003\001\004\001\025\001\017\001\029\001\
\018\001\029\001\078\000\079\000\025\001\081\000\031\001\083\000\
\089\000\090\000\091\000\092\000\088\000\022\001\027\001\145\000\
\027\001\026\001\033\001\042\000\028\001\030\001\030\001\028\001\
\032\001\028\001\107\000\027\001\023\001\110\000\111\000\112\000\
\055\000\031\001\025\001\029\001\003\001\009\001\119\000\006\001\
\029\001\064\000\025\001\066\000\125\000\012\001\009\001\014\001\
\124\000\130\000\073\000\132\000\023\001\022\001\021\001\029\001\
\029\001\026\001\139\000\140\000\141\000\030\001\143\000\025\001\
\145\000\146\000\147\000\001\001\028\001\000\001\001\001\002\001\
\003\001\004\001\028\001\023\001\023\001\027\001\159\000\160\000\
\011\001\012\001\158\000\014\001\107\000\016\001\015\001\023\001\
\010\001\020\001\029\001\000\001\001\001\002\001\003\001\004\001\
\029\001\028\001\023\001\030\001\027\001\032\001\007\001\012\001\
\025\001\014\001\011\001\031\001\009\001\000\001\029\001\016\001\
\029\001\029\001\019\001\020\001\007\001\029\001\010\001\028\001\
\011\001\030\001\145\000\032\001\056\000\016\001\033\001\020\000\
\019\001\020\001\027\000\118\000\125\000\081\000\065\000\132\000\
\102\000\159\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\009\001\010\001\013\001\010\001\013\001\
\010\001\255\255\018\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\024\001\024\001\024\001\029\001\030\001\031\001\
\004\001\033\001\255\255\255\255\008\001\009\001\010\001\255\255\
\255\255\013\001\255\255\255\255\255\255\255\255\018\001\255\255\
\255\255\255\255\255\255\255\255\024\001\025\001\255\255\255\255\
\255\255\029\001\255\255\031\001\255\255\033\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\255\255\255\255\255\255\
\018\001\255\255\255\255\255\255\255\255\255\255\024\001\025\001\
\255\255\255\255\255\255\029\001\255\255\031\001\255\255\033\001"

let yynames_const = "\
  ARRAY\000\
  BEGIN\000\
  CONST\000\
  DO\000\
  ELSE\000\
  END\000\
  FUNC\000\
  IF\000\
  IN\000\
  LET\000\
  OF\000\
  PROC\000\
  RECORD\000\
  THEN\000\
  TYPE\000\
  VAR\000\
  WHILE\000\
  DOT\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  BECOMES\000\
  IS\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LCURLY\000\
  RCURLY\000\
  EOF\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  CHARLITERAL\000\
  IDENTIFIER\000\
  OPERATOR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'Command) in
    Obj.repr(
# 40 "Parser.mly"
                            ( Program({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 335 "Parser.ml"
               : Ast.astProgram))
; (fun parser_env ->
    Obj.repr(
# 41 "Parser.mly"
                            ( ErrorReporter.reportError "Command expected here." (rhs_start_pos(1)); 
                              raise Parse_error )
# 342 "Parser.ml"
               : Ast.astProgram))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'single_Command) in
    Obj.repr(
# 47 "Parser.mly"
                                          ( _1 )
# 349 "Parser.ml"
               : 'Command))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'single_Command) in
    let _3 = (peek_val parser_env 0 : 'Command) in
    Obj.repr(
# 48 "Parser.mly"
                                          ( SequentialCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 357 "Parser.ml"
               : 'Command))
; (fun parser_env ->
    Obj.repr(
# 51 "Parser.mly"
                                                                      ( EmptyCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}) )
# 363 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Vname) in
    let _3 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 52 "Parser.mly"
                                                                      ( AssignCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 371 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'Identifier) in
    let _3 = (peek_val parser_env 1 : 'Actual_Parameter_Sequence) in
    Obj.repr(
# 53 "Parser.mly"
                                                                      ( CallCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 379 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'Command) in
    Obj.repr(
# 54 "Parser.mly"
                                                                      ( _2 )
# 386 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Declaration) in
    let _4 = (peek_val parser_env 0 : 'single_Command) in
    Obj.repr(
# 55 "Parser.mly"
                                                                      ( LetCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 394 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'Expression) in
    let _4 = (peek_val parser_env 2 : 'single_Command) in
    let _6 = (peek_val parser_env 0 : 'single_Command) in
    Obj.repr(
# 56 "Parser.mly"
                                                                      ( IfCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _6) )
# 403 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Expression) in
    let _4 = (peek_val parser_env 0 : 'single_Command) in
    Obj.repr(
# 57 "Parser.mly"
                                                                      ( WhileCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 411 "Parser.ml"
               : 'single_Command))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'secondary_Expression) in
    Obj.repr(
# 62 "Parser.mly"
                                                             ( _1 )
# 418 "Parser.ml"
               : 'Expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Declaration) in
    let _4 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 63 "Parser.mly"
                                                             ( LetExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 426 "Parser.ml"
               : 'Expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'Expression) in
    let _4 = (peek_val parser_env 2 : 'Expression) in
    let _6 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 64 "Parser.mly"
                                                             ( IfExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _6) )
# 435 "Parser.ml"
               : 'Expression))
; (fun parser_env ->
    Obj.repr(
# 65 "Parser.mly"
                                                             ( ErrorReporter.reportError "Expression expected here." (rhs_start_pos(1)); 
                                                               raise Parse_error )
# 442 "Parser.ml"
               : 'Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 70 "Parser.mly"
                                                                       ( _1 )
# 449 "Parser.ml"
               : 'secondary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'secondary_Expression) in
    let _2 = (peek_val parser_env 1 : 'Operator) in
    let _3 = (peek_val parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 71 "Parser.mly"
                                                                       ( BinaryExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _2, _3) )
# 458 "Parser.ml"
               : 'secondary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Integer_Literal) in
    Obj.repr(
# 74 "Parser.mly"
                                                                       ( IntegerExpression({pos=rhs_start_pos(1); run=NullRuntimeEntity}, _1) )
# 465 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Character_Literal) in
    Obj.repr(
# 75 "Parser.mly"
                                                                       ( CharacterExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity},_1) )
# 472 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Vname) in
    Obj.repr(
# 76 "Parser.mly"
                                                                       ( VnameExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 479 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'Identifier) in
    let _3 = (peek_val parser_env 1 : 'Actual_Parameter_Sequence) in
    Obj.repr(
# 77 "Parser.mly"
                                                                       ( CallExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 487 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'Operator) in
    let _2 = (peek_val parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 78 "Parser.mly"
                                                                       ( UnaryExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _2) )
# 495 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'Expression) in
    Obj.repr(
# 79 "Parser.mly"
                                                                       ( _2 )
# 502 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'Record_Aggregate) in
    Obj.repr(
# 80 "Parser.mly"
                                                                       ( RecordExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 509 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'Array_Aggregate) in
    Obj.repr(
# 81 "Parser.mly"
                                                                       ( ArrayExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 516 "Parser.ml"
               : 'primary_Expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Identifier) in
    let _3 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 85 "Parser.mly"
                                                                  ( SingleRecordAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 524 "Parser.ml"
               : 'Record_Aggregate))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'Identifier) in
    let _3 = (peek_val parser_env 2 : 'Expression) in
    let _5 = (peek_val parser_env 0 : 'Record_Aggregate) in
    Obj.repr(
# 86 "Parser.mly"
                                                                  ( MultipleRecordAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3, _5) )
# 533 "Parser.ml"
               : 'Record_Aggregate))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 90 "Parser.mly"
                                                  ( SingleArrayAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 540 "Parser.ml"
               : 'Array_Aggregate))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Expression) in
    let _3 = (peek_val parser_env 0 : 'Array_Aggregate) in
    Obj.repr(
# 91 "Parser.mly"
                                                  ( MultipleArrayAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 548 "Parser.ml"
               : 'Array_Aggregate))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Identifier) in
    Obj.repr(
# 95 "Parser.mly"
                                          ( SimpleVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 555 "Parser.ml"
               : 'Vname))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Vname) in
    let _3 = (peek_val parser_env 0 : 'Identifier) in
    Obj.repr(
# 96 "Parser.mly"
                                          ( DotVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 563 "Parser.ml"
               : 'Vname))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'Vname) in
    let _3 = (peek_val parser_env 1 : 'Expression) in
    Obj.repr(
# 97 "Parser.mly"
                                          ( SubscriptVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 571 "Parser.ml"
               : 'Vname))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'single_Declaration) in
    Obj.repr(
# 101 "Parser.mly"
                                                      ( _1 )
# 578 "Parser.ml"
               : 'Declaration))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Declaration) in
    let _3 = (peek_val parser_env 0 : 'single_Declaration) in
    Obj.repr(
# 102 "Parser.mly"
                                                      ( SequentialDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 586 "Parser.ml"
               : 'Declaration))
; (fun parser_env ->
    Obj.repr(
# 103 "Parser.mly"
                                                      ( ErrorReporter.reportError "Declaration expected here." (rhs_start_pos(1)); 
                                                        raise Parse_error )
# 593 "Parser.ml"
               : 'Declaration))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Identifier) in
    let _4 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 107 "Parser.mly"
                                                                                                             ( ConstDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 601 "Parser.ml"
               : 'single_Declaration))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Identifier) in
    let _4 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 108 "Parser.mly"
                                                                                                             ( VarDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 609 "Parser.ml"
               : 'single_Declaration))
; (fun parser_env ->
    let _2 = (peek_val parser_env 5 : 'Identifier) in
    let _4 = (peek_val parser_env 3 : 'Formal_Parameter_Sequence) in
    let _7 = (peek_val parser_env 0 : 'single_Command) in
    Obj.repr(
# 109 "Parser.mly"
                                                                                                             ( ProcDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _7) )
# 618 "Parser.ml"
               : 'single_Declaration))
; (fun parser_env ->
    let _2 = (peek_val parser_env 7 : 'Identifier) in
    let _4 = (peek_val parser_env 5 : 'Formal_Parameter_Sequence) in
    let _7 = (peek_val parser_env 2 : 'Type_denoter) in
    let _9 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 110 "Parser.mly"
                                                                                                             ( FuncDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _7, _9) )
# 628 "Parser.ml"
               : 'single_Declaration))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Identifier) in
    let _4 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 111 "Parser.mly"
                                                                                                             ( TypeDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 636 "Parser.ml"
               : 'single_Declaration))
; (fun parser_env ->
    Obj.repr(
# 115 "Parser.mly"
                                                            ( EmptyFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}) )
# 642 "Parser.ml"
               : 'Formal_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'proper_Formal_Parameter_Sequence) in
    Obj.repr(
# 116 "Parser.mly"
                                                            ( _1 )
# 649 "Parser.ml"
               : 'Formal_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Formal_Parameter) in
    Obj.repr(
# 119 "Parser.mly"
                                                                                          ( SingleFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity},_1) )
# 656 "Parser.ml"
               : 'proper_Formal_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Formal_Parameter) in
    let _3 = (peek_val parser_env 0 : 'proper_Formal_Parameter_Sequence) in
    Obj.repr(
# 120 "Parser.mly"
                                                                                          ( MultipleFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 664 "Parser.ml"
               : 'proper_Formal_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Identifier) in
    let _3 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 123 "Parser.mly"
                                                                                             ( ConstFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 672 "Parser.ml"
               : 'Formal_Parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Identifier) in
    let _4 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 124 "Parser.mly"
                                                                                             ( VarFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 680 "Parser.ml"
               : 'Formal_Parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'Identifier) in
    let _4 = (peek_val parser_env 1 : 'Formal_Parameter_Sequence) in
    Obj.repr(
# 125 "Parser.mly"
                                                                                             ( ProcFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 688 "Parser.ml"
               : 'Formal_Parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 5 : 'Identifier) in
    let _4 = (peek_val parser_env 3 : 'Formal_Parameter_Sequence) in
    let _7 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 126 "Parser.mly"
                                                                                             ( FuncFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _7) )
# 697 "Parser.ml"
               : 'Formal_Parameter))
; (fun parser_env ->
    Obj.repr(
# 130 "Parser.mly"
                                                            ( EmptyActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}) )
# 703 "Parser.ml"
               : 'Actual_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'proper_Actual_Parameter_Sequence) in
    Obj.repr(
# 131 "Parser.mly"
                                                            ( _1 )
# 710 "Parser.ml"
               : 'Actual_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Actual_Parameter) in
    Obj.repr(
# 134 "Parser.mly"
                                                                                          ( SingleActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 717 "Parser.ml"
               : 'proper_Actual_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Actual_Parameter) in
    let _3 = (peek_val parser_env 0 : 'proper_Actual_Parameter_Sequence) in
    Obj.repr(
# 135 "Parser.mly"
                                                                                          ( MultipleActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 725 "Parser.ml"
               : 'proper_Actual_Parameter_Sequence))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Expression) in
    Obj.repr(
# 138 "Parser.mly"
                                  ( ConstActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 732 "Parser.ml"
               : 'Actual_Parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'Vname) in
    Obj.repr(
# 139 "Parser.mly"
                                  ( VarActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 739 "Parser.ml"
               : 'Actual_Parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'Identifier) in
    Obj.repr(
# 140 "Parser.mly"
                                  ( ProcActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 746 "Parser.ml"
               : 'Actual_Parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'Identifier) in
    Obj.repr(
# 141 "Parser.mly"
                                  ( FuncActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 753 "Parser.ml"
               : 'Actual_Parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'Identifier) in
    Obj.repr(
# 145 "Parser.mly"
                                                    ( SimpleTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 760 "Parser.ml"
               : 'Type_denoter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'Integer_Literal) in
    let _4 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 146 "Parser.mly"
                                                    ( ArrayTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 768 "Parser.ml"
               : 'Type_denoter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'Record_Type_denoter) in
    Obj.repr(
# 147 "Parser.mly"
                                                    ( RecordTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 775 "Parser.ml"
               : 'Type_denoter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'Identifier) in
    let _3 = (peek_val parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 150 "Parser.mly"
                                                                             ( SingleFieldTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 783 "Parser.ml"
               : 'Record_Type_denoter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'Identifier) in
    let _3 = (peek_val parser_env 2 : 'Type_denoter) in
    let _5 = (peek_val parser_env 0 : 'Record_Type_denoter) in
    Obj.repr(
# 151 "Parser.mly"
                                                                             ( MultipleFieldTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3, _5) )
# 792 "Parser.ml"
               : 'Record_Type_denoter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 155 "Parser.mly"
                            ( IntegerLiteral({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 799 "Parser.ml"
               : 'Integer_Literal))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 159 "Parser.mly"
                               ( CharacterLiteral({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 806 "Parser.ml"
               : 'Character_Literal))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 163 "Parser.mly"
                       ( Identifier({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 813 "Parser.ml"
               : 'Identifier))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 167 "Parser.mly"
                   ( Operator({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 820 "Parser.ml"
               : 'Operator))
(* Entry parseProgram *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let parseProgram (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Ast.astProgram)
;;
