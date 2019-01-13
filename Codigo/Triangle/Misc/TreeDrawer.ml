(*
This program was made by Luis Leopoldo Pérez at March 12, 2006.
This program was repaired, completed, verified and validated by students
of ITCR at 2018.
Abstract Syntax Tree Drawing Library for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Christian León Guevara
@author Gabriel Ramírez Ramírez

Last modification: January, 2019
*)

open Ast
open Printf

let transform_operator o = 
  if ((String.compare o "<") == 0) then
    "&lt;"
  else if ((String.compare o "<=") == 0) then
    "&lt;="
  else o
  
let rec write_program t chan = match t with
  Null_program -> output_string chan "<Null_program/>\n"
| Program (_,c) -> 
    output_string chan "<Program>\n";
    write_command c chan;
	output_string chan "</Program>\n"
  

and write_command t chan = match t with
  Empty_command (_) -> output_string chan "<Empty_command/>\n"
| Assign_command (_,v,e) -> 
    output_string chan "<Assign_command>\n";
	write_vname v chan;
	write_expression e chan;
	output_string chan "</Assign_command>\n"
| Call_command (_,i,aps) -> 
    output_string chan "<Call_command>\n";
	write_identifier i chan;
	write_actual_parameter_sequence aps chan;
	output_string chan "</Call_command>\n"
| Sequential_command (_,c1,c2) -> 
    output_string chan "<Sequential_command>\n";
	write_command c1 chan;
	write_command c2 chan;
	output_string chan "</Sequential_command>\n"
| Let_command (_,d,c) -> 
    output_string chan "<Let_command>\n";
	write_declaration d chan;
	write_command c chan;
	output_string chan "</Let_command>\n"
| If_command (_,e,c1,c2) -> 
    output_string chan "<If_command>\n";
	write_expression e chan;
	write_command c1 chan;
	write_command c2 chan;
	output_string chan "</If_command>\n"
| While_command (_,e,c) -> 
	output_string chan "<While_command>\n";
	write_expression e chan;
	write_command c chan;
	output_string chan "</While_command>\n"

and write_expression t chan = match t with
  Empty_expression (_) -> output_string chan "<Empty_expression/>\n"
| Integer_expression (_,il) -> 
	output_string chan "<Integer_expression>\n";
	writeInteger_literal il chan;
	output_string chan "</Integer_expression>\n"
| Character_expression (_,cl) ->
	output_string chan "<Character_expression>\n";
	writeCharacter_literal cl chan;
	output_string chan "</Character_expression>\n"
| Vname_expression (_,v) -> 
	output_string chan "<Vname_expression>\n";
	write_vname v chan;
	output_string chan "</Vname_expression>\n"
| Call_expression (_,i,aps) -> 
	output_string chan "<Call_expression>\n";
	write_identifier i chan;
	write_actual_parameter_sequence aps chan;
	output_string chan "</Call_expression>\n"
| If_expression (_,e1,e2,e3) -> 
	output_string chan "<If_expression>\n";
	write_expression e1 chan;
	write_expression e2 chan;
	write_expression e2 chan;
	output_string chan "</If_expression>\n"
| Let_expression (_,d,e) -> 
	output_string chan "<Let_expression>\n";
	write_declaration d chan;
	write_expression e chan;
	output_string chan "</Let_expression>\n"
| Unary_expression (_,o,e) -> 
	output_string chan "<Unary_expression>\n";
	write_operator o chan;
	write_expression e chan;
	output_string chan "</Unary_expression>\n"
| Binary_expression (_,e1,o,e2) -> 
	output_string chan "<Binary_expression>\n";
	write_expression e1 chan;
	write_operator o chan;
	write_expression e2 chan;
	output_string chan "</Binary_expression>\n"
| Array_expression (_,aa) -> 
	output_string chan "<Array_expression>\n";
	write_array_aggregate aa chan;
	output_string chan "</Array_expression>\n"	
| Record_expression (_,ra) -> 
	output_string chan "<Record_expression>\n";
	write_record_aggregate ra chan;
	output_string chan "</Record_expression>\n"	
| Checked_expression (e,t) -> 
	output_string chan ("<Checked_expression>\n");
	write_expression e chan;
	write_type_denoter t chan;
	output_string chan "</Checked_expression>\n"                                        

and write_array_aggregate t chan = match t with
  Single_array_aggregate (_,e) -> 
    output_string chan "<Single_array_aggregate>\n";
	write_expression e chan;
	output_string chan "</Single_array_aggregate>\n"
| Multiple_array_aggregate (_,e,aa) -> 
	output_string chan "<Multiple_array_aggregate>\n";
	write_expression e chan;
	write_array_aggregate aa chan;
	output_string chan "</Multiple_array_aggregate>\n"
| Checked_array_aggregate (aa,i) -> 
	output_string chan (
	  "<Checked_array_aggregate elementCount=\"" ^ string_of_int i ^ "\">\n"
	);
	write_array_aggregate aa chan;
	output_string chan "</Checked_array_aggregate>\n"

and write_record_aggregate t chan = match t with
  Single_record_aggregate (_,i,e) -> 
    output_string chan "<Single_record_aggregate>\n";
	write_identifier i chan;
	write_expression e chan;
	output_string chan "</Single_record_aggregate>\n"
| Multiple_record_aggregate (_,i,e,ra) -> 
	output_string chan "<Multiple_record_aggregate>\n";
	write_identifier i chan;
	write_expression e chan;
	write_record_aggregate ra chan;
	output_string chan "</Multiple_record_aggregate>\n"
| Checked_record_aggregate (ra,t) -> 
	output_string chan ("<Checked_record_aggregate>\n");
	write_record_aggregate ra chan;
	write_field_type_denoter t chan;
	output_string chan "</Checked_record_aggregate>\n"

and write_vname t chan = match t with
  Simple_vname (_,i) -> 
    output_string chan "<Simple_vname>\n";
	write_identifier i chan;
	output_string chan "</Simple_vname>\n"	
| Dot_vname (_,v,i) -> 
	output_string chan "<Dot_vname>\n";
	write_vname v chan;
	write_identifier i chan;
	output_string chan "</Dot_vname>\n"
| Subscript_vname (_,v,e) -> 
	output_string chan "<Subscript_vname>\n";
	write_vname v chan;
	write_expression e chan;
	output_string chan "</Subscript_vname>\n"
| Checked_vname (v,vr,ix,os,t) -> 
	output_string chan (
	  "<Checked_vname variable=\"" 
	  ^ (string_of_bool vr) ^ "\" indexed=\"" 
	  ^ (string_of_bool ix) ^ "\" offset=\"" 
	  ^ (string_of_int os) ^ "\">\n"
	);
	write_vname v chan;
	write_type_denoter t chan;
	output_string chan "</Checked_vname>\n"

and write_declaration t chan = match t with
  Null_declaration -> output_string chan "</Null_declaration>\n";
| Const_declaration (_,i,e) -> 
	output_string chan "<Const_declaration>\n";
	write_identifier i chan;
	write_expression e chan;
	output_string chan "</Const_declaration>\n"
| Var_declaration (_,i,t) -> 
	output_string chan "<Var_declaration>\n";
	write_identifier i chan;
	write_type_denoter t chan;
	output_string chan "</Var_declaration>\n"
| Proc_declaration (_,i,fps,c) -> 
	output_string chan "<Proc_declaration>\n";
	write_identifier i chan;
	write_formal_parameter_sequence fps chan;
	write_command c chan;
	output_string chan "</Proc_declaration>\n"
| Func_declaration (_,i,fps,t,e) -> 
	output_string chan "<Func_declaration>\n";
	write_identifier i chan;
	write_formal_parameter_sequence fps chan;
	write_type_denoter t chan;
	write_expression e chan;
	output_string chan "</Func_declaration>\n"
| Type_declaration (_,i,t) -> 
	output_string chan "<Type_declaration>\n";
	write_identifier i chan;
	write_type_denoter t chan;
	output_string chan "</Type_declaration>\n"
| Unary_operator_declaration (_,o,t1,t2) -> 
	output_string chan "<Unary_operator_declaration>\n";
	write_operator o chan;
	write_type_denoter t1 chan;
	write_type_denoter t2 chan;
	output_string chan "</Unary_operator_declaration>\n"
| Binary_operator_declaration (_,o,t1,t2,t3) -> 
	output_string chan "<Binary_operator_declaration>\n";
	write_operator o chan;
	write_type_denoter t1 chan;
	write_type_denoter t2 chan;
	write_type_denoter t3 chan;
	output_string chan "</Binary_operator_declaration>\n"                                                
| Formal_parameter_declaration(_,fp) -> 
	output_string chan "<Formal_parameter_declaration>\n";
	write_formal_parameter fp chan;
	output_string chan "</Formal_parameter_declaration>\n"	
| Sequential_declaration (_,d1,d2) -> 
	output_string chan "<Sequential_declaration>\n";
	write_declaration d1 chan;
	write_declaration d2 chan;
	output_string chan "</Sequential_declaration>\n"

and write_formal_parameter t chan = match t with
  Const_formal_parameter (_,i,t) -> 
    output_string chan "<Const_formal_parameter>\n";
	write_identifier i chan;
	write_type_denoter t chan;
	output_string chan "</Const_formal_parameter>\n"
| Var_formal_parameter (_,i,t) -> 
	output_string chan "<Var_formal_parameter>\n";
	write_identifier i chan;
	write_type_denoter t chan;
	output_string chan "</Var_formal_parameter>\n"
| Proc_formal_parameter (_,i,fps) -> 
	output_string chan "<Proc_formal_parameter>\n";
	write_identifier i chan;
	write_formal_parameter_sequence fps chan;
	output_string chan "</Proc_formal_parameter>\n"
| Func_formal_parameter (_,i,fps,t) -> 
	output_string chan "<Func_formal_parameter>\n";
	write_identifier i chan;
	write_formal_parameter_sequence fps chan;
	write_type_denoter t chan;
	output_string chan "</Func_formal_parameter>\n"

and write_actual_parameter t chan = match t with
  Const_actual_parameter (_,e) -> 
    output_string chan "<Const_actual_parameter>\n";
	write_expression e chan;
	output_string chan "</Const_actual_parameter>\n"
| Var_actual_parameter (_,v) -> 
    output_string chan "<Var_actual_parameter>\n";
	write_vname v chan;
	output_string chan "</Var_actual_parameter>\n"
| Proc_actual_parameter (_,i) -> 
    output_string chan "<Proc_actual_parameter>\n";
	write_identifier i chan;
	output_string chan "</Proc_actual_parameter>\n"
| Func_actual_parameter (_,i) -> 
    output_string chan "<Func_actual_parameter>\n";
	write_identifier i chan;
	output_string chan "</Func_actual_parameter>\n"
	
and write_formal_parameter_sequence t chan = match t with
  Empty_formal_parameter_sequence (_) -> 
    output_string chan "<Empty_formal_parameter_sequence/>\n"

| Single_formal_parameter_sequence (_,fp) -> 
    output_string chan "<Single_formal_parameter_sequence>\n";
	write_formal_parameter fp chan;
	output_string chan "</Single_formal_parameter_sequence>\n"
| Multiple_formal_parameter_sequence (_,fp,fps) -> 
    output_string chan "<Multiple_formal_parameter_sequence>\n";
	write_formal_parameter fp chan;
	write_formal_parameter_sequence fps chan;
	output_string chan "</Multiple_formal_parameter_sequence>\n"

and write_actual_parameter_sequence t chan = match t with
  Empty_actual_parameter_sequence (_) -> 
    output_string chan "<Empty_actual_parameter_sequence/>\n"
| Single_actual_parameter_sequence (_,ap) -> 
    output_string chan "<Single_actual_parameter_sequence>\n";
	write_actual_parameter ap chan;
	output_string chan "</Single_actual_parameter_sequence>\n"
| Multiple_actual_parameter_sequence (_,ap,aps) -> 
    output_string chan "<Multiple_actual_parameter_sequence>\n";
	write_actual_parameter ap chan;
	write_actual_parameter_sequence aps chan;
	output_string chan "</Multiple_actual_parameter_sequence>\n"

and write_type_denoter t chan = match t with
  Null_type_denoter -> output_string chan "<Null_type_denoter/>\n"
| Error_type_denoter (_) -> output_string chan "<Error_type_denoter/>\n"
| Any_type_denoter (_) -> output_string chan "<Any_type_denoter/>\n"
| Simple_type_denoter (_,i) -> 
    output_string chan "<Simple_type_denoter>\n";
	write_identifier i chan;
	output_string chan "</Simple_type_denoter>\n"
| Array_type_denoter (_,il,t) -> 
    output_string chan "<Array_type_denoter>\n";
	writeInteger_literal il chan;
	write_type_denoter t chan;
	output_string chan "</Array_type_denoter>\n"
| Record_type_denoter (_,ft) -> 
    output_string chan "<Record_type_denoter>\n";
	write_field_type_denoter ft chan;
	output_string chan "</Record_type_denoter>\n"	
| Bool_type_denoter (_) -> output_string chan "<Bool_type_denoter/>\n"
| Int_type_denoter (_) -> output_string chan "<Int_type_denoter/>\n"
| Char_type_denoter (_) -> output_string chan "<Char_type_denoter/>\n"

and write_field_type_denoter t chan = match t with
  Single_field_type_denoter (_,i,t) -> 
    output_string chan "<Single_field_type_denoter>\n";
	write_identifier i chan;
	write_type_denoter t chan;
	output_string chan "</Single_field_type_denoter>\n"
| Multiple_field_type_denoter (_,i,t,ft) -> 
	output_string chan "<Multiple_field_type_denoter>\n";
	write_identifier i chan;
	write_type_denoter t chan;
	write_field_type_denoter ft chan;
	output_string chan "</Multiple_field_type_denoter>\n"

and writeInteger_literal t chan = match t with
  Integer_literal (_,str) -> 
    output_string chan ("<Integer_literal value=\"" ^ str ^ "\"/>\n")

and writeCharacter_literal t chan = match t with
  Character_literal (_,str) -> 
    output_string chan ("<Character_literal value=\"" ^ str ^ "\"/>\n")

and write_identifier t chan = match t with 
  Identifier (_,str) -> 
	output_string chan ("<Identifier value=\"" ^ str ^ "\"/>\n")
| Checked_identifier (i,d) -> 
    output_string chan ("<Checked_identifier>\n");
	write_identifier i chan;
	output_string chan "</Checked_identifier>\n"

and write_operator t chan = match t with
  Operator (_,str) -> 
    output_string chan ("<Operator value=\"" ^ (transform_operator str) ^ "\"/>\n")
| Checked_operator (o,d) -> 
    output_string chan "<Checked_operator>\n";
	write_operator o chan;
	output_string chan "</Checked_operator>\n"
	
let write_XML_tree astree fname = 
  try
    let chan = open_out fname in
      output_string chan "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
	  write_program astree chan;
	  close_out_noerr chan
  with Sys_error s -> printf "Couldn't write XML tree file. (%s)\n" s