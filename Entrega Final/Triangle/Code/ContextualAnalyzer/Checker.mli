(*
This program was made by Luis Leopoldo Pérez at March 12, 2006.
This program was repaired, completed, verified and validated by students
of ITCR at 2018.
Contextual Analyzer for Caml-Triangle
Interface file                                  

@author Luis Leopoldo Pérez
@author Christian León Guevara
@author Gabriel Ramírez Ramírez

Last modification: January, 2019
*)

open Parser
open Ast

(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
val check_program: Ast.ast_program -> Ast.ast_program