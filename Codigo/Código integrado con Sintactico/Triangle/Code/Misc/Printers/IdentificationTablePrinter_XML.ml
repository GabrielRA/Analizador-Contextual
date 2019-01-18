open Printf
open String
open IdentificationTable


(** This function receives the AST to be printed and the name of the file
in which it will be written). It is responsible for creating the file *)

let buf = ref ("<?xml version=\"1.0\" standalone=\"yes\"?>\n")


let rec write_close_scope_declarations level decl_list =
  match decl_list with
    [] -> ()
  | _  -> 
    let decl = 
      (List.hd decl_list) in 
        buf := (!buf ^("<Declaration id=\"" ^ decl.id ^ "level=\"" ^ (string_of_int level)^ "\"/>" ));
    write_close_scope_declarations level (List.tl decl_list) 



let write_IDTable_XML_tree astree fname =
  try
    let chan = open_out fname in
  	  

  	  close_out_noerr chan

    (**
    write_program astree chan;
    *)
    
  with
    Sys_error s -> printf "Couldn't write XML tree file. (%s)\n" s

(** Note: The first argument of all printing functions is ignored because it
stores irrelevant information for printing. *)