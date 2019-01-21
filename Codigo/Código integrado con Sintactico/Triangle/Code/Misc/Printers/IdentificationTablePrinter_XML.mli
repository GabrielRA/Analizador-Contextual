(**
This program was written by students of ITCR in January 2019.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019..
Identification Table Printer in a XML file for Caml-Triangle
Interface file                                

@author Christian León Guevara
@author Gabriel Ramírez Ramírez
*)
open String
open Ast
open Id_entry

val write_close_scope_declarations:int -> id_entry list ->  unit
val write_open_scope_declarations:int -> unit
val write_file: string -> unit