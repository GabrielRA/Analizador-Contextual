open Token

val parse_program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast_program
