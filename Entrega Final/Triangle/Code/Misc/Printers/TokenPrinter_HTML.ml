(**
This program was written by students of ITCR in January 2019.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019..
Tokens Drawing Library in a HTML file for Caml-Triangle
Implementation file                                

@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)


open Printf
open String
open Token

(** This variable stores the first part of the container that stores each 
token *)
let first_print = ref "<div class=\"col-\">
      <div class=\"card text-white "

(** This variable stores the second part of the container that stores each
token *)
let second_print = ref " mb-3\" style=\"max-width: 20rem;\">
        <div class=\"card-header text-white bg-dark\">"

(** This variable stores the third part of the container that stores each
token *)
let third_print = ref "</div>
        <div class=\"card-body\">
          <h5 class=\"card-title\">"

(** This variable stores the fourth part of the container that stores each
token *)
let fourth_print = ref "</h5>
        </div>
      </div>
    </div>"

(** This function calls each of the parts of the container in addition to 
completing the missing elements, returning the string with the container 
of the token *)
let token_to_str r = (
  match r with
    Int_literal(a) ->
    !first_print ^ "bg-secondary" ^ !second_print ^ "IntLiteral" ^ !third_print
    ^ a ^ !fourth_print
  | Char_literal(a) ->
    !first_print ^ "bg-success" ^ !second_print ^ "CharLiteral" ^ !third_print
    ^ a ^ !fourth_print
  | Identifier(a) ->
    !first_print ^ "bg-danger" ^ !second_print ^ "Identifier" ^ !third_print
    ^ a ^ !fourth_print
  | Operator(a) ->
    !first_print ^ "bg-warning" ^ !second_print ^ "Operator" ^ !third_print
    ^ a ^ !fourth_print
  | Array ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "array" ^ !fourth_print
  | Begin ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "begin" ^ !fourth_print
  | Const ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "const" ^ !fourth_print
  | Do ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "do" ^ !fourth_print
  | Else ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "else" ^ !fourth_print
  | End ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "end" ^ !fourth_print
  | Func ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "func" ^ !fourth_print
  | If ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "if" ^ !fourth_print
  | In ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "in" ^ !fourth_print
  | Let ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "let" ^ !fourth_print
  | Of ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "of" ^ !fourth_print
  | Proc ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "proc" ^ !fourth_print
  | Record ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "record" ^ !fourth_print
  | Then ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "then" ^ !fourth_print
  | Type ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "type" ^ !fourth_print
  | Var ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "var" ^ !fourth_print
  | While ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "while" ^ !fourth_print
  | Dot ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "." ^ !fourth_print
  | Colon ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ ":" ^ !fourth_print
  | Semicolon ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ ";" ^ !fourth_print
  | Comma ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "," ^ !fourth_print
  | Becomes ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ ":=" ^ !fourth_print
  | Is ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "~" ^ !fourth_print
  | Lparen ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "(" ^ !fourth_print
  | Rparen ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ ")" ^ !fourth_print
  | Lbracket ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "[" ^ !fourth_print
  | Rbracket ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "]" ^ !fourth_print
  | Lcurly ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "{" ^ !fourth_print
  | Rcurly ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "}" ^ !fourth_print
  | Eof ->
    !first_print ^ "bg-primary" ^ !second_print ^ "Token" ^ !third_print 
    ^ "end of file" ^ !fourth_print) 

(** This function receives the scanner function, the buffer of the file
being scanned and the handler of the file in which it is being written.
It is responsible for filling the body of the HTML. *)
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
    let header1 = 
    "<!DOCTYPE html>
    <html lang=\"es\">
    <head>
    <title>Tokens HTML result</title>
    <meta charset=\"utf-8\" />
    <link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/boots\
      trap/4.1.3/css/bootstrap.min.css\" integrity=\"sha384-MCw98/SFnGE8fJT3\
      GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO\" crossorigin=\"anony\
      mous\">
    </head>
    <body>
    <header>
    <h1>Tokens</h1>
    <p>The result of the generation of tokens is the following:</p>
    </header>
    <div class=\"container\">
    <div class=\"row\">" in
    output_string chan header1;

    print_tokens_aux scanner lex_buf chan;
    let header2 =
    "</div>
    </div>
    <script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\" integri\
    ty=\"sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6\
    jizo\" crossorigin=\"anonymous\"></script>
    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/um\
    d/popper.min.js\" integrity=\"sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn6\
    89aCwoqbBJiSnjAK/l8WvCWPIPm49\" crossorigin=\"anonymous\"></script>
    <script src=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/boot\
    strap.min.js\" integrity=\"sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241r\
    YiqJxyMiZ6OW/JmZQ5stwEULTy\" crossorigin=\"anonymous\"></script>
    </body>
    </html>" in
    output_string chan header2;
    close_out_noerr chan
  with
    Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s