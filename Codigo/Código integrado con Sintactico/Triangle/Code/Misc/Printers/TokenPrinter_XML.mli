(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Tokens Drawing Library in a XML file for Caml-Triangle
Interface file                                  

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

open Token

(** Write the tokens in an XML file. (Receive: the scaner 
function, the buffer of the file being scanned and the name 
of the output file). *) 
val print_tokens:
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string -> unit
