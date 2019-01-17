(**
This program was originally written by Luis Leopoldo Pérez on March 13, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Lexical Analyzer for Caml-Triangle
Interface file                                    

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)


open Token


(** Reads a token from the buffer and returns it *)
val scan_token: Lexing.lexbuf -> token
