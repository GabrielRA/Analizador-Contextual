(*
This program was made by Luis Leopoldo P�rez at March 12, 2006.
This program was repaired, completed, verified and validated by students
of ITCR at 2018.
Contextual Analyzer for Caml-Triangle
Interface file                                  

@author Luis Leopoldo P�rez
@author Christian Le�n Guevara
@author Gabriel Ram�rez Ram�rez

Last modification: January, 2019
*)

open Parser
open Ast

(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
val check_program: Ast.ast_program -> Ast.ast_program