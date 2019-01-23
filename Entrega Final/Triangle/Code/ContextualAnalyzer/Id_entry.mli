(*
This program was made by Luis Leopoldo Pérez at March 12, 2006.
This program was repaired, completed, verified and validated by students
of ITCR at 2018.
Id Entry for Caml-Triangle
Interface file                                  

@author Luis Leopoldo Pérez
@author Christian León Guevara
@author Gabriel Ramírez Ramírez

Last modification: January, 2019
*)
open Ast
open RuntimeEntity


type id_entry = {mutable id: string; mutable attr: ast_declaration ref; mutable levl: int}