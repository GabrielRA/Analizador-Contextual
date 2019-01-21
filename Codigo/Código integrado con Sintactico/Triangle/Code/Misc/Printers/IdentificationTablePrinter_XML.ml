(**
This program was written by students of ITCR in January 2019.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019..
Identification Table Printer in a XML file for Caml-Triangle
Implementation file                                

@author Christian León Guevara
@author Gabriel Ramírez Ramírez
*)
open String
open Ast
open Id_entry
open Printf

(** This function receives the AST to be printed and the name of the file
in which it will be written). It is responsible for creating the file *)

let buf = ref ("<?xml version=\"1.0\" standalone=\"yes\"?>\n")

let write_open_scope_declarations level =
  buf := (!buf ^ "<Nivel"^(string_of_int level)^">\n")


let rec write_close_scope_declarations level decl_list =
  match decl_list with
    [] -> buf := (!buf ^("</Nivel"^ (string_of_int level) ^">\n"))
  | _  -> 
    let decl = 
      (List.hd decl_list) in 
        buf := (!buf ^("<Declaration id=\"" ^ decl.id ^ "\" level=\"" ^ (string_of_int level)^ "\"/>\n" ));
        write_close_scope_declarations level (List.tl decl_list) 

let write_file file_name =
  let chan = open_out file_name in
    output_string chan !buf;
    close_out_noerr chan
(** Note: The first argument of all printing functions is ignored because it
stores irrelevant information for printing. *)