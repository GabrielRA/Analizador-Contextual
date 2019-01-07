(* ------------------------------------------------------ *)
(* Abstract Syntax Tree Drawing Library for Caml-Triangle *)
(* Implementation file                                    *)
(*                                                        *)
(* (c) 2006 Luis Leopoldo Pérez.                          *)
(* Last modification: March 12, 2006                      *)
(* ------------------------------------------------------ *)

open Ast
open Printf

let transformOperator o = 
    if ((String.compare o "<") == 0) then
       "&lt;"
    else if ((String.compare o "<=") == 0) then
       "&lt;="
    else o
  
let rec writeProgram t chan = match t with
    Null_program   -> output_string chan "<Null_program/>\n"

  | Program (_,c) -> output_string chan "<Program>\n";
                     writeCommand c chan;
                     output_string chan "</Program>\n"
  

and writeCommand t chan = match t with
    Empty_command      (_)         -> output_string chan "<Empty_command/>\n"

  | Assign_command     (_,v,e)     -> output_string chan "<Assign_command>\n";
                                     writeVname v chan;
                                     writeExpression e chan;
                                     output_string chan "</Assign_command>\n"

  | Call_command       (_,i,aps)   -> output_string chan "<Call_command>\n";
                                     writeIdentifier i chan;
                                     writeActualParameterSequence aps chan;
                                     output_string chan "</Call_command>\n"

  | Sequential_command (_,c1,c2)   -> output_string chan "<Sequential_command>\n";
                                     writeCommand c1 chan;
                                     writeCommand c2 chan;
                                     output_string chan "</Sequential_command>\n"

  | LetCommand        (_,d,c)     -> output_string chan "<LetCommand>\n";
                                     writeDeclaration d chan;
                                     writeCommand c chan;
                                     output_string chan "</LetCommand>\n"

  | If_command         (_,e,c1,c2) -> output_string chan "<If_command>\n";
                                     writeExpression e chan;
                                     writeCommand c1 chan;
                                     writeCommand c2 chan;
                                     output_string chan "</If_command>\n"

  | While_command      (_,e,c)     -> output_string chan "<While_command>\n";
                                     writeExpression e chan;
                                     writeCommand c chan;
                                     output_string chan "</While_command>\n"

and writeExpression t chan = match t with
    Empty_expression     (_)          -> output_string chan "<Empty_expression/>\n"

  | Integer_expression   (_,il)       -> output_string chan "<Integer_expression>\n";
                                        writeInteger_literal il chan;
                                        output_string chan "</Integer_expression>\n"

  | Character_expression (_,cl)       -> output_string chan "<Character_expression>\n";
                                        writeCharacter_literal cl chan;
                                        output_string chan "</Character_expression>\n"

  | Vname_expression     (_,v)        -> output_string chan "<Vname_expression>\n";
                                        writeVname v chan;
                                        output_string chan "</Vname_expression>\n"

  | Call_expression      (_,i,aps)    -> output_string chan "<Call_expression>\n";
                                        writeIdentifier i chan;
                                        writeActualParameterSequence aps chan;
                                        output_string chan "</Call_expression>\n"

  | If_expression        (_,e1,e2,e3) -> output_string chan "<If_expression>\n";
                                        writeExpression e1 chan;
                                        writeExpression e2 chan;
                                        writeExpression e2 chan;
                                        output_string chan "</If_expression>\n"

  | Let_expression       (_,d,e)      -> output_string chan "<Let_expression>\n";
                                        writeDeclaration d chan;
                                        writeExpression e chan;
                                        output_string chan "</Let_expression>\n"

  | Unary_expression     (_,o,e)      -> output_string chan "<Unary_expression>\n";
                                        writeOperator o chan;
                                        writeExpression e chan;
                                        output_string chan "</Unary_expression>\n"

  | Binary_expression    (_,e1,o,e2)  -> output_string chan "<Binary_expression>\n";
                                        writeExpression e1 chan;
                                        writeOperator o chan;
                                        writeExpression e2 chan;
                                        output_string chan "</Binary_expression>\n"

  | Array_expression     (_,aa)       -> output_string chan "<Array_expression>\n";
                                        writeArrayAggregate aa chan;
                                        output_string chan "</Array_expression>\n"

  | Record_expression    (_,ra)       -> output_string chan "<Record_expression>\n";
                                        writeRecordAggregate ra chan;
                                        output_string chan "</Record_expression>\n"

  | Checked_expression   (e,t)        -> output_string chan ("<Checked_expression>\n");
                                        writeExpression e chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</Checked_expression>\n"
                                        

and writeArrayAggregate t chan = match t with
    Single_array_aggregate   (_,e)    -> output_string chan "<Single_array_aggregate>\n";
                                       writeExpression e chan;
                                       output_string chan "</Single_array_aggregate>\n"

  | Multiple_array_aggregate (_,e,aa) -> output_string chan "<Multiple_array_aggregate>\n";
                                       writeExpression e chan;
                                       writeArrayAggregate aa chan;
                                       output_string chan "</Multiple_array_aggregate>\n"

  | CheckedArrayAggregate  (aa,i)   -> output_string chan ("<CheckedArrayAggregate elementCount=\"" ^ string_of_int i ^ "\">\n");
                                       writeArrayAggregate aa chan;
                                       output_string chan "</CheckedArrayAggregate>\n"

and writeRecordAggregate t chan = match t with
    SingleRecordAggregate   (_,i,e)    -> output_string chan "<SingleRecordAggregate>\n";
                                          writeIdentifier i chan;
                                          writeExpression e chan;
                                          output_string chan "</SingleRecordAggregate>\n"

  | MultipleRecordAggregate (_,i,e,ra) -> output_string chan "<MultipleRecordAggregate>\n";
                                          writeIdentifier i chan;
                                          writeExpression e chan;
                                          writeRecordAggregate ra chan;
                                          output_string chan "</MultipleRecordAggregate>\n"

  | Checked_record_aggregate  (ra,t)     -> output_string chan ("<Checked_record_aggregate>\n");
                                          writeRecordAggregate ra chan;
                                          writeFieldTypeDenoter t chan;
                                          output_string chan "</Checked_record_aggregate>\n"

and writeVname t chan = match t with
    Simple_vname (_,i)            -> output_string chan "<Simple_vname>\n";
                                    writeIdentifier i chan;
                                    output_string chan "</Simple_vname>\n"

  | Dot_vname (_,v,i)             -> output_string chan "<Dot_vname>\n";
                                    writeVname v chan;
                                    writeIdentifier i chan;
                                    output_string chan "</Dot_vname>\n"

  | Subscript_vname (_,v,e)       -> output_string chan "<Subscript_vname>\n";
                                    writeVname v chan;
                                    writeExpression e chan;
                                    output_string chan "</Subscript_vname>\n"

  | Checked_vname (v,vr,ix,os,t)  -> output_string chan ("<Checked_vname variable=\"" ^ (string_of_bool vr) ^ "\" indexed=\"" ^ (string_of_bool ix) ^ "\" offset=\"" ^ (string_of_int os) ^ "\">\n");
                                    writeVname v chan;
                                    writeTypeDenoter t chan;
                                    output_string chan "</Checked_vname>\n"


and writeDeclaration t chan = match t with
    Null_declaration                          -> output_string chan "</Null_declaration>\n";
  | Const_declaration          (_,i,e)        -> output_string chan "<Const_declaration>\n";
                                                writeIdentifier i chan;
                                                writeExpression e chan;
                                                output_string chan "</Const_declaration>\n"

  | Var_declaration            (_,i,t)        -> output_string chan "<Var_declaration>\n";
                                                writeIdentifier i chan;
                                                writeTypeDenoter t chan;
                                                output_string chan "</Var_declaration>\n"

  | Proc_declaration           (_,i,fps,c)    -> output_string chan "<Proc_declaration>\n";
                                                writeIdentifier i chan;
                                                writeFormalParameterSequence fps chan;
                                                writeCommand c chan;
                                                output_string chan "</Proc_declaration>\n"

  | Func_declaration           (_,i,fps,t,e)  -> output_string chan "<Func_declaration>\n";
                                                writeIdentifier i chan;
                                                writeFormalParameterSequence fps chan;
                                                writeTypeDenoter t chan;
                                                writeExpression e chan;
                                                output_string chan "</Func_declaration>\n"

  | Type_declaration           (_,i,t)        -> output_string chan "<Type_declaration>\n";
                                                writeIdentifier i chan;
                                                writeTypeDenoter t chan;
                                                output_string chan "</Type_declaration>\n"

  | Unary_operator_declaration  (_,o,t1,t2)    -> output_string chan "<Unary_operator_declaration>\n";
                                                writeOperator o chan;
                                                writeTypeDenoter t1 chan;
                                                writeTypeDenoter t2 chan;
                                                output_string chan "</Unary_operator_declaration>\n"

  | Binary_operator_declaration (_,o,t1,t2,t3) -> output_string chan "<Binary_operator_declaration>\n";
                                                writeOperator o chan;
                                                writeTypeDenoter t1 chan;
                                                writeTypeDenoter t2 chan;
                                                writeTypeDenoter t3 chan;
                                                output_string chan "</Binary_operator_declaration>\n"
                                                
  | Formal_parameter_declaration(_,fp)         -> output_string chan "<Formal_parameter_declaration>\n";
                                                writeFormalParameter fp chan;
                                                output_string chan "</Formal_parameter_declaration>\n"

  | Sequential_declaration     (_,d1,d2)      -> output_string chan "<Sequential_declaration>\n";
                                                writeDeclaration d1 chan;
                                                writeDeclaration d2 chan;
                                                output_string chan "</Sequential_declaration>\n"


and writeFormalParameter t chan = match t with
    Const_formal_parameter (_,i,t)     -> output_string chan "<Const_formal_parameter>\n";
                                        writeIdentifier i chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</Const_formal_parameter>\n"

  | Var_formal_parameter (_,i,t)       -> output_string chan "<Var_formal_parameter>\n";
                                        writeIdentifier i chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</Var_formal_parameter>\n"

  | Proc_formal_parameter (_,i,fps)    -> output_string chan "<Proc_formal_parameter>\n";
                                        writeIdentifier i chan;
                                        writeFormalParameterSequence fps chan;
                                        output_string chan "</Proc_formal_parameter>\n"

  | Func_formal_parameter (_,i,fps,t)  -> output_string chan "<Func_formal_parameter>\n";
                                        writeIdentifier i chan;
                                        writeFormalParameterSequence fps chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</Func_formal_parameter>\n"


and writeActualParameter t chan = match t with
    Const_actual_parameter (_,e) -> output_string chan "<Const_actual_parameter>\n";
                                  writeExpression e chan;
                                  output_string chan "</Const_actual_parameter>\n"

  | Var_actual_parameter (_,v)   -> output_string chan "<Var_actual_parameter>\n";
                                  writeVname v chan;
                                  output_string chan "</Var_actual_parameter>\n"

  | Proc_actual_parameter (_,i)  -> output_string chan "<Proc_actual_parameter>\n";
                                  writeIdentifier i chan;
                                  output_string chan "</Proc_actual_parameter>\n"

  | Func_actual_parameter (_,i)  -> output_string chan "<Func_actual_parameter>\n";
                                  writeIdentifier i chan;
                                  output_string chan "</Func_actual_parameter>\n"

and writeFormalParameterSequence t chan = match t with
    Empty_formal_parameter_sequence  (_)          -> output_string chan "<Empty_formal_parameter_sequence/>\n"

  | Single_formal_parameter_sequence (_,fp)       -> output_string chan "<Single_formal_parameter_sequence>\n";
                                                  writeFormalParameter fp chan;
                                                  output_string chan "</Single_formal_parameter_sequence>\n"

  | Multiple_formal_parameter_sequence (_,fp,fps) -> output_string chan "<Multiple_formal_parameter_sequence>\n";
                                                  writeFormalParameter fp chan;
                                                  writeFormalParameterSequence fps chan;
                                                  output_string chan "</Multiple_formal_parameter_sequence>\n"

and writeActualParameterSequence t chan = match t with
    EmptyActualParameterSequence (_)           -> output_string chan "<EmptyActualParameterSequence/>\n"

  | SingleActualParameterSequence (_,ap)       -> output_string chan "<SingleActualParameterSequence>\n";
                                                  writeActualParameter ap chan;
                                                  output_string chan "</SingleActualParameterSequence>\n"

  | MultipleActualParameterSequence (_,ap,aps) -> output_string chan "<MultipleActualParameterSequence>\n";
                                                  writeActualParameter ap chan;
                                                  writeActualParameterSequence aps chan;
                                                  output_string chan "</MultipleActualParameterSequence>\n"

and writeTypeDenoter t chan = match t with
    Null_type_denoter           -> output_string chan "<Null_type_denoter/>\n"
  | Error_type_denoter (_)      -> output_string chan "<Error_type_denoter/>\n"

  | Any_type_denoter (_)        -> output_string chan "<Any_type_denoter/>\n"

  | SimpleTypeDenoter (_,i)   -> output_string chan "<SimpleTypeDenoter>\n";
                                 writeIdentifier i chan;
                                 output_string chan "</SimpleTypeDenoter>\n"

  | Array_type_denoter (_,il,t) -> output_string chan "<Array_type_denoter>\n";
                                 writeInteger_literal il chan;
                                 writeTypeDenoter t chan;
                                 output_string chan "</Array_type_denoter>\n"

  | Record_type_denoter (_,ft)  -> output_string chan "<Record_type_denoter>\n";
                                 writeFieldTypeDenoter ft chan;
                                 output_string chan "</Record_type_denoter>\n"

  | Bool_type_denoter (_)       -> output_string chan "<Bool_type_denoter/>\n"

  | Int_type_denoter (_)        -> output_string chan "<Int_type_denoter/>\n"

  | Char_type_denoter (_)       -> output_string chan "<Char_type_denoter/>\n"


and writeFieldTypeDenoter t chan = match t with
    Single_field_type_denoter (_,i,t)      -> output_string chan "<Single_field_type_denoter>\n";
                                           writeIdentifier i chan;
                                           writeTypeDenoter t chan;
                                           output_string chan "</Single_field_type_denoter>\n"

  | Multiple_field_type_denoter (_,i,t,ft) -> output_string chan "<Multiple_field_type_denoter>\n";
                                           writeIdentifier i chan;
                                           writeTypeDenoter t chan;
                                           writeFieldTypeDenoter ft chan;
                                           output_string chan "</Multiple_field_type_denoter>\n"

and writeInteger_literal t chan = match t with
    Integer_literal (_,str) -> output_string chan ("<Integer_literal value=\"" ^ str ^ "\"/>\n")

and writeCharacter_literal t chan = match t with
    Character_literal (_,str) -> output_string chan ("<Character_literal value=\"" ^ str ^ "\"/>\n")

and writeIdentifier t chan = match t with 
    Identifier (_,str)        -> output_string chan ("<Identifier value=\"" ^ str ^ "\"/>\n")

  | Checked_identifier (i,d)   -> output_string chan ("<Checked_identifier>\n");
                                 writeIdentifier i chan;
                                 output_string chan "</Checked_identifier>\n"

and writeOperator t chan = match t with
    Operator (_,str)      -> output_string chan ("<Operator value=\"" ^ (transformOperator str) ^ "\"/>\n")
  | Checked_operator (o,d) -> output_string chan "<Checked_operator>\n";
                             writeOperator o chan;
                             output_string chan "</Checked_operator>\n"

let writeXMLTree astree fname = 
    try
      let chan = open_out fname in
    	  output_string chan "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
          writeProgram astree chan;
          close_out_noerr chan
    with Sys_error s -> printf "Couldn't write XML tree file. (%s)\n" s