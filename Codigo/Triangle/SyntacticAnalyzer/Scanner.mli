(* ---------------------------------- *)
(* Lexical Analyzer for Caml-Triangle *)
(* Interface file                     *)
(*                                    *)
(* (c) 2006 Luis Leopoldo Pérez.      *)
(* Last modification: March 13, 2006  *)
(* ---------------------------------- *)

open Parser


(* Reads a token from the buffer and returns it *)
val scanToken: Lexing.lexbuf -> token
