(* ------------------------------------- *)
(* Contextual Analyzer for Caml-Triangle *)
(* Interface file                        *)
(*                                       *)
(* (c) 2006 Luis Leopoldo Pérez.         *)
(* Last modification: March 12, 2006     *)
(* ------------------------------------- *)

open Parser
open Ast

(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
val check_program: Ast.ast_program -> Ast.ast_program