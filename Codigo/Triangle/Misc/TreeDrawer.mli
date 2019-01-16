(* ------------------------------------------------------ *)
(* Abstract Syntax Tree Drawing Library for Caml-Triangle *)
(* Interface file                                         *)
(*                                                        *)
(* (c) 2006 Luis Leopoldo Pérez.                          *)
(* Last modification: March 12, 2006                      *)
(* ------------------------------------------------------ *)

open Ast

(* Writes an entire abstract syntax tree into an XML file *)
val write_XML_tree: ast_program -> string -> unit