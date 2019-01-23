(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Tokens Drawing Library in a XML file for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)


open Printf
open String
open Token

(** This function given a char returns its equivalent in XML (this for reasons 
of compatibility in the print). *)
let transform_operator o = (
  match o with 
    '<' -> "&lt;"
  | '!' -> "&#33;"        
  | '\"' -> "&#34;"        
  | '#' -> "&#35;"        
  | '$' -> "&#36;"        
  | '%' -> "&#37;"        
  | '&' -> "&#38;"        
  | '\'' -> "&#39;"        
  | '(' -> "&#40;"        
  | ')' -> "&#41;"        
  | '*' -> "&#42;"        
  | '+' -> "&#43;"        
  | ',' -> "&#44;"        
  | '-' -> "&#45;"        
  | '.' -> "&#46;"        
  | '/' -> "&#47;"        
  | ':' -> "&#58;"        
  | ';' -> "&#59;"         
  | '=' -> "&#61;"        
  | '>' -> "&#62;"        
  | '?' -> "&#63;"        
  | '@' -> "&#64;"        
  | '[' -> "&#91;"        
  | '\\' -> "&#92;"        
  | ']' -> "&#93;"        
  | '^' -> "&#94;"        
  | '_' -> "&#95;"        
  | '`' -> "&#96;"        
  | '{' -> "&#123;"       
  | '|' -> "&#124;"      
  | '}' -> "&#125;"     
  | '~' -> "&#126;"
  | _ ->  (String.make 1 o) )

(** This function transforms a normal string to a compatible one to any format. 
This function receives the string to be changed, the character transformation
function, the lower limit to be transformed, the upper limit to be
transformed and the result (it is advised to use "") *)
let rec transform_string string_base transform_char index limit result =
  if (index = limit) then
    result
  else
    let char_to_transform = string_base.[index] in
    let transformed_char = transform_char char_to_transform in
    let new_result = result ^ transformed_char in
    transform_string string_base transform_char (index + 1) limit new_result


(** This function given a string returns its equivalent in XML *)
let clean_String str = (
	transform_string str transform_operator 0 (String.length str) "")

(** This function given to the token returns its equivalent in string xml, 
thus allowing to fill the body of the program. *)
let token_to_str r = (
  match r with
    Int_literal(a) ->
    "<IntLiteral value=\"" ^ (clean_String a) ^ "\"></IntLiteral>"
  | Char_literal(a) ->
    "<CharLiteral value=\"" ^ (clean_String a) ^ "\"></CharLiteral>"
  | Identifier(a) ->
    "<Identifier value=\""  ^ (clean_String a) ^ "\"></Identifier>"
  | Operator(a) ->
    "<Operator value=\""    ^ (clean_String a) ^ "\"></Operator>"
  | Array -> "<Token value=\"array\"></Token>"
  | Begin -> "<Token value=\"begin\"></Token>"
  | Const -> "<Token value=\"const\"></Token>"
  | Do -> "<Token value=\"do\"></Token>"
  | Else -> "<Token value=\"else\"></Token>"
  | End -> "<Token value=\"end\"></Token>"
  | Func -> "<Token value=\"func\"></Token>"
  | If -> "<Token value=\"if\"></Token>"
  | In -> "<Token value=\"in\"></Token>"
  | Let -> "<Token value=\"let\"></Token>"
  | Of -> "<Token value=\"of\"></Token>"
  | Proc -> "<Token value=\"proc\"></Token>"
  | Record -> "<Token value=\"record\"></Token>"
  | Then -> "<Token value=\"then\"></Token>"
  | Type -> "<Token value=\"type\"></Token>"
  | Var -> "<Token value=\"var\"></Token>"
  | While -> "<Token value=\"while\"></Token>"
  | Dot -> "<Token value=\".\"></Token>"
  | Colon -> "<Token value=\":\"></Token>"
  | Semicolon -> "<Token value=\";\"></Token>"
  | Comma -> "<Token value=\",\"></Token>"
  | Becomes -> "<Token value=\":=\"></Token>"
  | Is -> "<Token value=\"~\"></Token>"
  | Lparen -> "<Token value=\"(\"></Token>"
  | Rparen -> "<Token value=\")\"></Token>"
  | Lbracket -> "<Token value=\"[\"></Token>"
  | Rbracket -> "<Token value=\"]\"></Token>"
  | Lcurly -> "<Token value=\"{\"></Token>"
  | Rcurly -> "<Token value=\"}\"></Token>"
  | Eof -> "<Token value=\"end of file\"></Token>")


(** This function receives the function of scanner, the buffer of the
file that is being scanned and the handler of the file in which it is being
written). It is responsible for completing the body of the XML. *)
let rec print_tokens_aux scanner lex_buf chan = 
  let current_token = scanner lex_buf in
  let token_string = token_to_str current_token in
    output_string chan token_string;
    output_string chan "\n";
  if (current_token <> Eof) then
    print_tokens_aux scanner lex_buf chan

(** This function receives the function of scanner, the buffer of the file 
that is being scanned and the name of the file in which it will be written. 
Is responsible for creating the file, fill in the header and footer. *)
let print_tokens scanner lex_buf file_name =
  try
    let chan = open_out file_name in
    output_string chan "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
    output_string chan "<Program>";
    print_tokens_aux scanner lex_buf chan;
    output_string chan "</Program>";
    close_out_noerr chan
  with 
    Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s