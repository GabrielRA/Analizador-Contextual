(**
This program was written by students of ITCR in January 2019.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Tokens Drawing Library in a plain text file for Caml-Triangle
Implementation file                                

@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

open Printf
open String
open Token

(** This function given a token returns its equivalent in string. *)
let token_to_str r = (
  match r with
    Int_literal(a) -> a
  | Char_literal(a) -> a
  | Identifier(a) -> a
  | Operator(a) -> a
  | Array -> "array"
  | Begin -> "begin"
  | Const -> "const"
  | Do -> "do"
  | Else -> "else"
  | End -> "end"
  | Func -> "func"
  | If -> "if"
  | In -> "in"
  | Let -> "let"
  | Of -> "of"
  | Proc -> "proc"
  | Record -> "record"
  | Then -> "then"
  | Type -> "type"
  | Var -> "var"
  | While -> "while"
  | Dot -> "."
  | Colon -> ":"
  | Semicolon -> ";"
  | Comma -> ","
  | Becomes -> ":="
  | Is -> "~"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lcurly -> "{"
  | Rcurly -> "}"
  | Eof -> "end of file")


(** This function receives the scanner function, the buffer of the file 
being scanned and the handler of the file in which it is being written. 
It is responsible for filling the file with the tokens separated by pipes. *)
let rec print_tokens_aux scanner lex_buf chan = 
  let current_token = scanner lex_buf in
  let token_string = token_to_str current_token in
  output_string chan token_string;
  output_string chan "|";
  
  if (current_token <> Eof) then
    print_tokens_aux scanner lex_buf chan


(** This function receives the function of scanner, the buffer of the file 
that is being scanned and the name of the file in which it will be written. 
Is responsible for creating the file. *)
let print_tokens scanner lex_buf file_name =
  try
    let chan = open_out file_name in
    print_tokens_aux scanner lex_buf chan;
    close_out_noerr chan
  with 
    Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s