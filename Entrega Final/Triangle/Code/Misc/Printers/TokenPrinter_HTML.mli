(**
This program was written by students of ITCR in January 2019.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Tokens Drawing Library in a HTML file for Caml-Triangle
Interface file                                 

@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)


open Token

(** Write the tokens in an HTML file. (Receive: the scaner 
function, the buffer of the file being scanned and the name 
of the output file). *) 
val print_tokens:
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string -> unit
