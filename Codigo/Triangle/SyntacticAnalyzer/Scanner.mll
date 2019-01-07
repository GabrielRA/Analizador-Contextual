{

(* ------------------------------------------------- *)
(* Lexer generator file (ocamllex) for Caml-Triangle *)
(*                                                   *)
(* (c) 2006 Luis Leopoldo Pérez.                     *)
(* Last modification: April 12, 2006                 *)
(* ------------------------------------------------- *)

 open Printf
 open Parser
 open ErrorReporter
 open Lexing


 exception LexingError

 (* Creates a hashtable - borrowed from the ocamllex samples *)
 let create_hashtable size init =
 let tbl = Hashtbl.create size in
   List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
   tbl

 (* Keywords hashtable - maps every keyword with its token *)
 let keyword_table =
    create_hashtable 29 [
	("array", ARRAY);
    	("begin", BEGIN);
    	("const", CONST);
    	("do", DO);
    	("else", ELSE);
    	("end", END);
    	("func", FUNC);
    	("if", IF);
    	("in", IN);
    	("let", LET);
    	("of", OF);
    	("proc", PROC);
    	("record", RECORD);
    	("then", THEN);
    	("type", TYPE);
    	("var", VAR);
    	("while", WHILE);
    	(".", DOT);
    	(":", COLON);
    	(";", SEMICOLON);
    	(",", COMMA);
    	(":=", BECOMES);
    	("~", IS);
    	("(", LPAREN);
    	(")", RPAREN);
    	("[", LBRACKET);
    	("]", RBRACKET);
    	("{", LCURLY);
    	("}", RCURLY)
 ]
 
}

(* Lexical definitions *)
let letter 		= ['a'-'z' 'A'-'Z']
let digit 		= ['0'-'9']
let op_character 	= ['+' '-' '*' '/' '=' '<' '>' '\\' '&' '@' '%' '^' '?']
let blank		= [ ' ' '\t' ]
let graphic		= letter
			| digit
			| op_character
                        | blank
			| [ '.' ':' ';' ',' '~' '(' ')' '[' ']' '{' '}' '_' '|' '!' '\'' '`' '"' '#' '$' ]
let end_of_line		= ('\n'|'\r')|('\r' '\n')|('\n' '\r')
let comment		= '!' (blank|graphic)+
let operator		= op_character op_character*
let identifier		= letter (letter|digit)*
let character_literal	= '\'' graphic '\''
let integer_literal	= digit digit*
let token 		= "array"
			| "begin"
			| "const"
			| "do"
			| "else"
			| "end"
			| "func"
			| "if"
			| "in"
			| "let"
			| "of"
			| "proc"
			| "record"
			| "then"
			| "type"
			| "var"
			| "while"
			| "."
			| ":"
			| ";"
			| ","
			| ":="
			| "~"
			| "("
			| ")"
			| "["
			| "]"
			| "{"
			| "}"

(* Scanning rules *)
rule scanToken 	= parse
		| token as param (* Tokens - look for every token in the hash table *)
			{
			 let tok = Hashtbl.find keyword_table param in
			 	tok;
			}
                | operator as param (* Operators *)
                        {
                         OPERATOR param
                        }
		| integer_literal as param (* Integer Literals *)
			{
				INTLITERAL param;
			}
		| character_literal as param (* Character Literals *)
			{
				CHARLITERAL param;
			}
		| identifier as param (* Identifiers *)
			{
				IDENTIFIER param;
			}
		| end_of_line (* EOL characters - increase the line counter *)
		        {
		                let cpos = lexbuf.lex_curr_p in
		                    let newpos = {pos_fname=cpos.pos_fname; 
		                                  pos_lnum=cpos.pos_lnum + 1;
		                                  pos_bol=cpos.pos_cnum;
		                                  pos_cnum=cpos.pos_cnum} in
		                        lexbuf.lex_curr_p <- newpos;
		                scanToken lexbuf
		        }
		| comment
		| blank   (* Blanks and comments - discard them *)
			{
				scanToken lexbuf
			}
		| eof     (* End of file character *)
			{
                                EOF
			}
		| _ as param (* Anything else is reported as unrecognized *)
			{
				reportError ("Unrecognized symbol '" ^ (Char.escaped param) ^ "'") lexbuf.lex_curr_p;
                                raise LexingError
			}