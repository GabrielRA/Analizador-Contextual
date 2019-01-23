{

(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Lexer generator file (ocamllex) for Caml-Triangle. 

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)



open Token
open ErrorReporter
open Lexing


exception LexingError

(** Creates a hashtable - borrowed from the ocamllex samples *)
let create_hashtable size init =
let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(** Keywords hashtable - maps every keyword with its token *)
let keyword_table = create_hashtable 29
  [
    ("array", Array);
    ("begin", Begin);
    ("const", Const);
    ("do", Do);
    ("else", Else);
    ("end", End);
    ("func", Func);
    ("if", If);
    ("in", In);
    ("let", Let);
    ("of", Of);
    ("proc", Proc);
    ("record", Record);
    ("then", Then);
    ("type", Type);
    ("var", Var);
    ("while", While);
    (".", Dot);
    (":", Colon);
    (";", Semicolon);
    (",", Comma);
    (":=", Becomes);
    ("~", Is);
    ("(", Lparen);
    (")", Rparen);
    ("[", Lbracket);
    ("]", Rbracket);
    ("{", Lcurly);
    ("}", Rcurly)
  ]
 
}

(** Lexical definitions *)
let end_of_line = ('\n'|'\r')|('\r' '\n')|('\n' '\r')

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let op_character = ['+' '-' '*' '/' '=' '<' '>' '\\' '&' '@' '%' '^' '?']

let blank = [ ' ' '\t' ]

let graphic =
    letter
  | digit
  | op_character
  | [' ' '\t']
  | ['.' ':' ';' ',' '~' '(' ')' '[' ']' '{' '}' '_' '|' '!' '\'' '`' '"' '#' '$']

let comment        = '!' graphic*
let operator    = op_character op_character*
let identifier    = letter (letter|digit)*
let character_literal  = '\'' graphic '\''
let integer_literal  = digit digit*
let token   =
    "array"
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

(** Scanning rules *)
rule scan_token   = parse
  | token as param (* Tokens - look for every token in the hash table *)
    {
      let tok = Hashtbl.find keyword_table param in
      tok;
    }
  | operator as param (* Operators *)
    {
      Operator param
    }
  | integer_literal as param (* Integer Literals *)
    {
      Int_literal param;
    }
  | character_literal as param (* Character Literals *)
    {
      Char_literal param;
    }
  | identifier as param (* Identifiers *)
    {
      Identifier param;
    }
  | end_of_line (* Eol characters - increase the line counter *)
      {
        let cpos = lexbuf.lex_curr_p in
        let newpos = 
          {pos_fname = cpos.pos_fname; 
           pos_lnum = cpos.pos_lnum + 1;
           pos_bol = cpos.pos_cnum;
           pos_cnum = cpos.pos_cnum} in
        lexbuf.lex_curr_p <- newpos;
        scan_token lexbuf
      }
  | comment 
  | blank   (* Blanks and comments - discard them *)
    {
      scan_token lexbuf
    }
  | eof (* End of file character *)
    {
      Eof
    }
  | _ as param (* Anything else is reported as unrecognized *)
    {
      let error_message = "Unrecognized symbol '" ^ (Char.escaped param) ^ "'" in
      report_error error_message lexbuf.lex_curr_p;
      raise LexingError
    }