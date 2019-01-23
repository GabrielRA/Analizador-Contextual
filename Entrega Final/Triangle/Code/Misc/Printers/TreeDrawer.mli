(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Abstract Syntax Tree Drawing Library for Caml-Triangle
Interface file                                  

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

open Ast

(** Writes an entire abstract syntax tree into an XML file *)
val write_x_m_l_tree: ast_program -> string -> unit