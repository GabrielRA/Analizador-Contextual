(* --------------------------------- *)
(* Code Generator for Caml-Triangle  *)
(* Interface file                    *)
(*                                   *)
(* (c) 2006 Luis Leopoldo Pérez.     *)
(* Last modification: March 12, 2006 *)
(* --------------------------------- *)

open Ast

(* Generates the machine code for the program and saves it in the specified file name *)
val encodeProgram: Ast.ast_program -> string -> unit

(* Writes the current table details to the specified XML file *)
val writeXMLTable: string -> unit