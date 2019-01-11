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

val parseProgram :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast_program
