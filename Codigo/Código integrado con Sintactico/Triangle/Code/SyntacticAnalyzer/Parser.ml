open Parsing;;
let _ = parse_error;;
# 2 "Parser.mly"

(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by students of ITCR in January 2019.
LParser generator file (ocamlyacc) for Caml-Triangle                                    

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)



open Ast
open Parsing
open ErrorReporter
open RuntimeEntity
open Token

(** This is the declaration of exception of program *)
exception Synt_error of string;;

let parse_error s = ()
# 65 "Parser.ml"
let yytransl_const = [|
  261 (* Array *);
  262 (* Begin *);
  263 (* Const *);
  264 (* Do *);
  265 (* Else *);
  266 (* End *);
  267 (* Func *);
  268 (* If *);
  269 (* In *);
  270 (* Let *);
  271 (* Of *);
  272 (* Proc *);
  273 (* Record *);
  274 (* Then *);
  275 (* Type *);
  276 (* Var *);
  277 (* While *);
  278 (* Dot *);
  279 (* Colon *);
  280 (* Semicolon *);
  281 (* Comma *);
  282 (* Becomes *);
  283 (* Is *);
  284 (* Lparen *);
  285 (* Rparen *);
  286 (* Lbracket *);
  287 (* Rbracket *);
  288 (* Lcurly *);
  289 (* Rcurly *);
  290 (* Eof *);
    0|]

let yytransl_block = [|
  257 (* Int_literal *);
  258 (* Char_literal *);
  259 (* Identifier *);
  260 (* Operator *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\014\000\014\000\015\000\015\000\004\000\004\000\004\000\
\008\000\008\000\008\000\016\000\016\000\016\000\016\000\016\000\
\018\000\018\000\019\000\019\000\020\000\020\000\020\000\020\000\
\020\000\007\000\007\000\021\000\021\000\022\000\022\000\022\000\
\022\000\017\000\017\000\017\000\017\000\023\000\023\000\012\000\
\013\000\006\000\011\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\000\000\003\000\004\000\003\000\004\000\
\006\000\004\000\001\000\001\000\004\000\006\000\001\000\001\000\
\003\000\001\000\001\000\001\000\004\000\002\000\003\000\003\000\
\003\000\003\000\005\000\001\000\003\000\001\000\003\000\004\000\
\001\000\003\000\001\000\004\000\004\000\007\000\009\000\004\000\
\000\000\001\000\001\000\003\000\003\000\004\000\005\000\007\000\
\001\000\000\000\001\000\001\000\003\000\001\000\002\000\002\000\
\002\000\001\000\004\000\003\000\001\000\003\000\005\000\001\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\066\000\000\000\000\000\000\000\000\000\
\068\000\000\000\002\000\000\000\000\000\000\000\015\000\064\000\
\065\000\067\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\018\000\019\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\031\000\005\000\000\000\000\000\000\000\
\000\000\054\000\000\000\051\000\000\000\000\000\000\000\023\000\
\000\000\025\000\000\000\024\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\008\000\034\000\010\000\032\000\
\057\000\056\000\000\000\030\000\006\000\000\000\000\000\013\000\
\029\000\000\000\000\000\021\000\036\000\049\000\000\000\000\000\
\000\000\000\000\000\000\042\000\000\000\000\000\061\000\000\000\
\000\000\058\000\040\000\037\000\053\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\027\000\000\000\000\000\000\000\045\000\
\000\000\044\000\000\000\000\000\000\000\060\000\000\000\000\000\
\046\000\000\000\038\000\059\000\000\000\000\000\047\000\000\000\
\000\000\000\000\039\000\063\000\048\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\024\000\074\000\026\000\075\000\038\000\
\027\000\028\000\029\000\030\000\031\000\054\000\052\000\039\000\
\123\000\115\000\116\000\117\000\076\000\077\000\138\000"

let yysindex = "\005\000\
\101\255\000\000\000\000\000\000\101\255\172\255\118\255\172\255\
\000\000\243\254\000\000\054\255\246\254\248\254\000\000\000\000\
\000\000\000\000\172\255\118\255\172\255\172\255\012\255\009\255\
\006\255\051\255\033\255\000\000\186\255\000\000\000\000\000\000\
\012\255\012\255\012\255\012\255\012\255\252\254\000\000\073\255\
\101\255\000\000\012\255\172\255\172\255\151\255\000\000\075\255\
\032\255\066\255\080\255\077\255\087\255\067\255\101\255\151\255\
\186\255\000\000\090\255\093\255\096\255\105\255\112\255\101\255\
\192\255\101\255\000\000\000\000\000\000\108\255\012\255\012\255\
\012\255\000\000\099\255\000\000\120\255\172\255\172\255\000\000\
\172\255\000\000\172\255\000\000\134\255\121\255\000\000\172\255\
\058\255\058\255\047\255\047\255\000\000\000\000\000\000\000\000\
\000\000\000\000\009\255\000\000\000\000\151\255\147\255\000\000\
\000\000\132\255\101\255\000\000\000\000\000\000\012\255\012\255\
\012\255\135\255\130\255\000\000\141\255\139\255\000\000\168\255\
\012\255\000\000\000\000\000\000\000\000\172\255\012\255\000\000\
\142\255\149\255\155\255\047\255\157\255\058\255\158\255\167\255\
\169\255\181\255\000\000\000\000\058\255\058\255\047\255\000\000\
\047\255\000\000\101\255\047\255\047\255\000\000\164\255\165\255\
\000\000\170\255\000\000\000\000\171\255\175\255\000\000\172\255\
\012\255\047\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\043\255\000\000\000\000\000\000\251\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\097\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\225\255\
\000\000\197\255\252\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\255\000\000\000\000\000\000\000\000\166\255\000\000\000\000\
\000\000\000\000\178\255\000\000\000\000\000\000\204\255\166\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\000\000\004\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\188\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\191\255\191\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\069\255\000\000\000\000\000\000\000\000\000\000\
\000\000\190\255\004\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\195\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\191\255\191\255\000\000\000\000\
\000\000\000\000\049\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\215\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\227\000\245\255\002\000\004\000\255\255\180\000\217\000\
\000\000\228\255\212\000\120\000\000\000\114\000\161\000\179\000\
\210\255\174\255\111\000\000\000\144\000\000\000\086\000"

let yytablesize = 286
let yytable = "\013\000\
\058\000\047\000\012\000\013\000\004\000\001\000\012\000\118\000\
\064\000\025\000\041\000\040\000\004\000\004\000\004\000\041\000\
\004\000\046\000\004\000\065\000\042\000\053\000\048\000\055\000\
\050\000\051\000\004\000\004\000\087\000\067\000\043\000\059\000\
\060\000\061\000\062\000\063\000\018\000\004\000\045\000\013\000\
\004\000\068\000\012\000\085\000\079\000\124\000\119\000\069\000\
\070\000\004\000\004\000\120\000\093\000\013\000\095\000\065\000\
\012\000\110\000\151\000\152\000\004\000\004\000\013\000\121\000\
\013\000\012\000\004\000\012\000\111\000\097\000\098\000\100\000\
\004\000\112\000\099\000\043\000\004\000\113\000\056\000\044\000\
\066\000\103\000\104\000\045\000\051\000\144\000\106\000\114\000\
\114\000\122\000\122\000\109\000\078\000\055\000\080\000\128\000\
\153\000\055\000\154\000\084\000\003\000\156\000\157\000\004\000\
\081\000\013\000\005\000\082\000\012\000\129\000\130\000\131\000\
\006\000\083\000\007\000\165\000\088\000\032\000\030\000\137\000\
\089\000\008\000\030\000\090\000\033\000\053\000\030\000\101\000\
\034\000\139\000\122\000\091\000\114\000\035\000\092\000\155\000\
\036\000\037\000\096\000\114\000\114\000\122\000\107\000\122\000\
\102\000\013\000\122\000\122\000\012\000\108\000\015\000\016\000\
\017\000\004\000\018\000\126\000\127\000\132\000\133\000\137\000\
\122\000\071\000\019\000\163\000\020\000\134\000\072\000\135\000\
\016\000\141\000\073\000\015\000\016\000\017\000\004\000\018\000\
\142\000\143\000\021\000\145\000\022\000\148\000\023\000\019\000\
\147\000\020\000\016\000\017\000\004\000\018\000\150\000\149\000\
\158\000\159\000\050\000\161\000\160\000\162\000\033\000\021\000\
\030\000\022\000\034\000\023\000\030\000\030\000\030\000\035\000\
\028\000\030\000\036\000\037\000\004\000\021\000\030\000\022\000\
\052\000\023\000\030\000\041\000\030\000\030\000\026\000\043\000\
\062\000\030\000\030\000\030\000\020\000\030\000\030\000\014\000\
\020\000\020\000\020\000\086\000\049\000\020\000\057\000\136\000\
\140\000\105\000\020\000\094\000\146\000\125\000\164\000\000\000\
\020\000\020\000\000\000\000\000\000\000\020\000\000\000\020\000\
\000\000\020\000\020\000\012\000\012\000\012\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\012\000\012\000\000\000\000\000\000\000\
\012\000\000\000\012\000\000\000\012\000\012\000"

let yycheck = "\001\000\
\029\000\010\001\001\000\005\000\010\001\001\000\005\000\090\000\
\013\001\006\000\024\001\008\000\009\001\010\001\003\001\024\001\
\013\001\028\001\024\001\024\001\034\001\023\000\019\000\018\001\
\021\000\022\000\010\001\024\001\057\000\041\000\022\001\033\000\
\034\000\035\000\036\000\037\000\004\001\034\001\030\001\041\000\
\024\001\043\000\041\000\055\000\013\001\092\000\000\001\044\000\
\045\000\003\001\034\001\005\001\064\000\055\000\066\000\024\001\
\055\000\000\001\141\000\142\000\003\001\013\001\064\000\017\001\
\066\000\064\000\024\001\066\000\011\001\071\000\072\000\073\000\
\024\001\016\001\073\000\022\001\034\001\020\001\028\001\026\001\
\008\001\078\000\079\000\030\001\081\000\132\000\083\000\089\000\
\090\000\091\000\092\000\088\000\018\001\025\001\029\001\107\000\
\143\000\029\001\145\000\033\001\000\001\148\000\149\000\003\001\
\025\001\107\000\006\001\031\001\107\000\111\000\112\000\113\000\
\012\001\027\001\014\001\162\000\027\001\000\001\022\001\121\000\
\028\001\021\001\026\001\028\001\007\001\127\000\030\001\029\001\
\011\001\126\000\132\000\027\001\134\000\016\001\023\001\147\000\
\019\001\020\001\031\001\141\000\142\000\143\000\009\001\145\000\
\025\001\147\000\148\000\149\000\147\000\029\001\000\001\001\001\
\002\001\003\001\004\001\009\001\025\001\023\001\029\001\161\000\
\162\000\011\001\012\001\160\000\014\001\025\001\016\001\029\001\
\001\001\028\001\020\001\000\001\001\001\002\001\003\001\004\001\
\028\001\023\001\028\001\023\001\030\001\015\001\032\001\012\001\
\027\001\014\001\001\001\002\001\003\001\004\001\010\001\023\001\
\029\001\029\001\029\001\025\001\027\001\023\001\007\001\028\001\
\004\001\030\001\011\001\032\001\008\001\009\001\010\001\016\001\
\031\001\013\001\019\001\020\001\009\001\028\001\018\001\030\001\
\029\001\032\001\022\001\029\001\024\001\025\001\033\001\029\001\
\010\001\029\001\030\001\031\001\004\001\033\001\034\001\005\000\
\008\001\009\001\010\001\056\000\020\000\013\001\027\000\120\000\
\127\000\081\000\018\001\065\000\134\000\102\000\161\000\255\255\
\024\001\025\001\255\255\255\255\255\255\029\001\255\255\031\001\
\255\255\033\001\034\001\008\001\009\001\010\001\255\255\255\255\
\013\001\255\255\255\255\255\255\255\255\018\001\255\255\255\255\
\255\255\255\255\255\255\024\001\025\001\255\255\255\255\255\255\
\029\001\255\255\031\001\255\255\033\001\034\001"

let yynames_const = "\
  Array\000\
  Begin\000\
  Const\000\
  Do\000\
  Else\000\
  End\000\
  Func\000\
  If\000\
  In\000\
  Let\000\
  Of\000\
  Proc\000\
  Record\000\
  Then\000\
  Type\000\
  Var\000\
  While\000\
  Dot\000\
  Colon\000\
  Semicolon\000\
  Comma\000\
  Becomes\000\
  Is\000\
  Lparen\000\
  Rparen\000\
  Lbracket\000\
  Rbracket\000\
  Lcurly\000\
  Rcurly\000\
  Eof\000\
  "

let yynames_block = "\
  Int_literal\000\
  Char_literal\000\
  Identifier\000\
  Operator\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    Obj.repr(
# 51 "Parser.mly"
                             ( Program({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 331 "Parser.ml"
               : Ast.ast_program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 56 "Parser.mly"
                                          ( _1 )
# 338 "Parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 57 "Parser.mly"
                                          ( Sequential_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 346 "Parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "Parser.mly"
                                                                      ( Empty_command({pos=rhs_start_pos(1);run=Null_runtime_entity}) )
# 352 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 61 "Parser.mly"
                                                                      ( Assign_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 360 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Actual_Parameter_Sequence) in
    Obj.repr(
# 62 "Parser.mly"
                                                                        ( Call_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 368 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    Obj.repr(
# 63 "Parser.mly"
                                                                      ( _2 )
# 375 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Declaration) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 64 "Parser.mly"
                                                                      ( Let_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 383 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'single_Command) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 65 "Parser.mly"
                                                                      ( If_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4, _6) )
# 392 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 66 "Parser.mly"
                                                                      ( While_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 400 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "Parser.mly"
                                                                      ( ErrorReporter.report_error "Cannot start a command." (rhs_start_pos(1)); 
                                                                        raise (Synt_error "Syntactical error") )
# 407 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'secondary_Expression) in
    Obj.repr(
# 72 "Parser.mly"
                                                             ( _1 )
# 414 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Declaration) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 73 "Parser.mly"
                                                             ( Let_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 422 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 74 "Parser.mly"
                                                             ( If_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4, _6) )
# 431 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "Parser.mly"
                                                             ( ErrorReporter.report_error "Cannot start a expression." (rhs_start_pos(1)); 
                                                               raise (Synt_error "Syntactical error") )
# 438 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 80 "Parser.mly"
                                                                       ( _1 )
# 445 "Parser.ml"
               : 'secondary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'secondary_Expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'R_Operator) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 81 "Parser.mly"
                                                                         ( Binary_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _2, _3) )
# 454 "Parser.ml"
               : 'secondary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Integer_Literal) in
    Obj.repr(
# 84 "Parser.mly"
                                                                       ( Integer_expression({pos=rhs_start_pos(1); run=Null_runtime_entity}, _1) )
# 461 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Character_Literal) in
    Obj.repr(
# 85 "Parser.mly"
                                                                       ( Character_expression({pos=rhs_start_pos(1);run=Null_runtime_entity},_1) )
# 468 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Vname) in
    Obj.repr(
# 86 "Parser.mly"
                                                                       ( Vname_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 475 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Actual_Parameter_Sequence) in
    Obj.repr(
# 87 "Parser.mly"
                                                                         ( Call_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 483 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'R_Operator) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 88 "Parser.mly"
                                                                         ( Unary_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _2) )
# 491 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expression) in
    Obj.repr(
# 89 "Parser.mly"
                                                                       ( _2 )
# 498 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Record_Aggregate) in
    Obj.repr(
# 90 "Parser.mly"
                                                                       ( Record_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2) )
# 505 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Array_Aggregate) in
    Obj.repr(
# 91 "Parser.mly"
                                                                       ( Array_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2) )
# 512 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 95 "Parser.mly"
                                                                    ( Single_record_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 520 "Parser.ml"
               : 'Record_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Record_Aggregate) in
    Obj.repr(
# 96 "Parser.mly"
                                                                    ( Multiple_record_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3, _5) )
# 529 "Parser.ml"
               : 'Record_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 100 "Parser.mly"
                                                  ( Single_array_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 536 "Parser.ml"
               : 'Array_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Array_Aggregate) in
    Obj.repr(
# 101 "Parser.mly"
                                                  ( Multiple_array_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 544 "Parser.ml"
               : 'Array_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'R_Identifier) in
    Obj.repr(
# 105 "Parser.mly"
                                            ( Simple_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 551 "Parser.ml"
               : 'Vname))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'R_Identifier) in
    Obj.repr(
# 106 "Parser.mly"
                                            ( Dot_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 559 "Parser.ml"
               : 'Vname))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Expression) in
    Obj.repr(
# 107 "Parser.mly"
                                          ( Subscript_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 567 "Parser.ml"
               : 'Vname))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_Declaration) in
    Obj.repr(
# 111 "Parser.mly"
                                                      ( _1 )
# 574 "Parser.ml"
               : 'Declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Declaration) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'single_Declaration) in
    Obj.repr(
# 112 "Parser.mly"
                                                      ( Sequential_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 582 "Parser.ml"
               : 'Declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "Parser.mly"
                                                      ( ErrorReporter.report_error "Cannot start a declaration." (rhs_start_pos(1)); 
                                                        raise (Synt_error "Syntactical error") )
# 589 "Parser.ml"
               : 'Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 117 "Parser.mly"
                                                                                                               ( Const_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 597 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 118 "Parser.mly"
                                                                                                               ( Var_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 605 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'Formal_Parameter_Sequence) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 119 "Parser.mly"
                                                                                                               ( Proc_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4, _7) )
# 614 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'Formal_Parameter_Sequence) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'Type_denoter) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 120 "Parser.mly"
                                                                                                               ( Func_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4, _7, _9) )
# 624 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 121 "Parser.mly"
                                                                                                               ( Type_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 632 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "Parser.mly"
                                                            ( Empty_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}) )
# 638 "Parser.ml"
               : 'Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Formal_Parameter_Sequence) in
    Obj.repr(
# 126 "Parser.mly"
                                                            ( _1 )
# 645 "Parser.ml"
               : 'Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Formal_Parameter) in
    Obj.repr(
# 129 "Parser.mly"
                                                                                          ( Single_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity},_1) )
# 652 "Parser.ml"
               : 'proper_Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Formal_Parameter) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Formal_Parameter_Sequence) in
    Obj.repr(
# 130 "Parser.mly"
                                                                                          ( Multiple_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 660 "Parser.ml"
               : 'proper_Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 133 "Parser.mly"
                                                                                               ( Const_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 668 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 134 "Parser.mly"
                                                                                               ( Var_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 676 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Formal_Parameter_Sequence) in
    Obj.repr(
# 135 "Parser.mly"
                                                                                               ( Proc_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 684 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'R_Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'Formal_Parameter_Sequence) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 136 "Parser.mly"
                                                                                               ( Func_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4, _7) )
# 693 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "Parser.mly"
                                                                                               ( ErrorReporter.report_error "Cannot start a formal parameter." (rhs_start_pos(1)); 
                                                                                                 raise (Synt_error "Syntactical error") )
# 700 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "Parser.mly"
                                                            ( Empty_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}) )
# 706 "Parser.ml"
               : 'Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Actual_Parameter_Sequence) in
    Obj.repr(
# 143 "Parser.mly"
                                                            ( _1 )
# 713 "Parser.ml"
               : 'Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Actual_Parameter) in
    Obj.repr(
# 146 "Parser.mly"
                                                                                          ( Single_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 720 "Parser.ml"
               : 'proper_Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Actual_Parameter) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Actual_Parameter_Sequence) in
    Obj.repr(
# 147 "Parser.mly"
                                                                                          ( Multiple_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 728 "Parser.ml"
               : 'proper_Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 150 "Parser.mly"
                                    ( Const_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 735 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Vname) in
    Obj.repr(
# 151 "Parser.mly"
                                    ( Var_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2) )
# 742 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'R_Identifier) in
    Obj.repr(
# 152 "Parser.mly"
                                    ( Proc_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2) )
# 749 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'R_Identifier) in
    Obj.repr(
# 153 "Parser.mly"
                                    ( Func_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2) )
# 756 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'R_Identifier) in
    Obj.repr(
# 157 "Parser.mly"
                                                    ( Simple_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 763 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Integer_Literal) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 158 "Parser.mly"
                                                    ( Array_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2, _4) )
# 771 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Record_Type_denoter) in
    Obj.repr(
# 159 "Parser.mly"
                                                    ( Record_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _2) )
# 778 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "Parser.mly"
                                                    ( ErrorReporter.report_error "Cannot start type denoter." (rhs_start_pos(1)); 
                                                      raise (Synt_error "Syntactical error") )
# 785 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 163 "Parser.mly"
                                                                               ( Single_field_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3) )
# 793 "Parser.ml"
               : 'Record_Type_denoter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'R_Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Type_denoter) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Record_Type_denoter) in
    Obj.repr(
# 164 "Parser.mly"
                                                                               ( Multiple_field_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1, _3, _5) )
# 802 "Parser.ml"
               : 'Record_Type_denoter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 168 "Parser.mly"
                             ( Integer_literal({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 809 "Parser.ml"
               : 'Integer_Literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "Parser.mly"
                                ( Character_literal({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 816 "Parser.ml"
               : 'Character_Literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 176 "Parser.mly"
                         ( Ast.Identifier({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 823 "Parser.ml"
               : 'R_Identifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 180 "Parser.mly"
                     ( Ast.Operator({pos=rhs_start_pos(1);run=Null_runtime_entity}, _1) )
# 830 "Parser.ml"
               : 'R_Operator))
(* Entry parse_program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse_program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.ast_program)
;;
