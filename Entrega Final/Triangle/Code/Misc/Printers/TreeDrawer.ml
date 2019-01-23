(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019
Abstract Syntax Tree Drawing Library for Caml-Triangle
Implementation file

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

open Ast
open Printf

(** This function given a char returns its equivalent in XML (this for reasons
of compatibility in the print). *)
let transform_operator o = (
  match o with
    '<' -> "&lt;"
  | '!' -> "&#33;"
  | '\"' -> "&#34;"
  | '#' -> "&#35;"
  | '$' -> "&#36;"
  | '%' -> "&#37;"
  | '&' -> "&#38;"
  | '\'' -> "&#39;"
  | '(' -> "&#40;"
  | ')' -> "&#41;"
  | '*' -> "&#42;"
  | '+' -> "&#43;"
  | ',' -> "&#44;"
  | '-' -> "&#45;"
  | '.' -> "&#46;"
  | '/' -> "&#47;"
  | ':' -> "&#58;"
  | ';' -> "&#59;"
  | '=' -> "&#61;"
  | '>' -> "&#62;"
  | '?' -> "&#63;"
  | '@' -> "&#64;"
  | '[' -> "&#91;"
  | '\\' -> "&#92;"
  | ']' -> "&#93;"
  | '^' -> "&#94;"
  | '_' -> "&#95;"
  | '`' -> "&#96;"
  | '{' -> "&#123;"
  | '|' -> "&#124;"
  | '}' -> "&#125;"
  | '~' -> "&#126;"
  | _ ->  (String.make 1 o) )


(** This function transforms a normal string to a compatible one to any format.
This function receives the string to be changed, the character transformation
function, the lower limit to be transformed, the upper limit to be
transformed and the result (it is advised to use "") *)
let rec transform_string string_base transform_char index limit result =
  if (index = limit) then
    result
  else
    let char_to_transform = string_base.[index] in
    let transformed_char = transform_char char_to_transform in
    let new_result = result ^ transformed_char in
    transform_string string_base transform_char (index + 1) limit new_result

(** This function given a string returns its equivalent in XML *)
let clean_String str = (
	transform_string str transform_operator 0 (String.length str) "")

(** Given a program, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
let rec write_program t chan =
  match t with    Null_program -> output_string chan "<NullProgram/>\n"
  | Program (_,c) ->
    output_string chan "<Program>\n";
    write_command c chan;
    output_string chan "</Program>\n"

(** Given a command, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and write_command t chan =
  match t with    Empty_command (_) ->
    output_string chan "<EmptyCommand/>\n"
  | Assign_command (_,v,e) ->
    output_string chan "<AssignCommand>\n";
    write_vname v chan;
    write_expression e chan;
    output_string chan "</AssignCommand>\n"
  | Call_command (_,i,aps) ->
    output_string chan "<CallCommand>\n";
    write_identifier i chan;
    write_actual_parameter_sequence aps chan;
    output_string chan "</CallCommand>\n"
  | Sequential_command (_,c1,c2) ->
    output_string chan "<SequentialCommand>\n";
    write_command c1 chan;
    write_command c2 chan;
    output_string chan "</SequentialCommand>\n"
  | Let_command (_,d,c) ->
    output_string chan "<LetCommand>\n";
    write_declaration d chan;
    write_command c chan;
    output_string chan "</LetCommand>\n"
  | If_command (_,e,c1,c2) ->
    output_string chan "<IfCommand>\n";
    write_expression e chan;
    write_command c1 chan;
    write_command c2 chan;
    output_string chan "</IfCommand>\n"
  | While_command (_,e,c) ->
    output_string chan "<WhileCommand>\n";
    write_expression e chan;
    write_command c chan;
    output_string chan "</WhileCommand>\n"

(** Given a expression, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and write_expression t chan =
  match t with    Empty_expression (_) ->
    output_string chan "<EmptyExpression/>\n"
  | Integer_expression (_,il) ->
    output_string chan "<IntegerExpression>\n";
    write_integer_literal il chan;
    output_string chan "</IntegerExpression>\n"
  | Character_expression (_,cl) ->
    output_string chan "<CharacterExpression>\n";
    write_character_literal cl chan;
    output_string chan "</CharacterExpression>\n"
  | Vname_expression (_,v) ->
    output_string chan "<VnameExpression>\n";
    write_vname v chan;
    output_string chan "</VnameExpression>\n"
  | Call_expression (_,i,aps) ->
    output_string chan "<CallExpression>\n";
    write_identifier i chan;
    write_actual_parameter_sequence aps chan;
    output_string chan "</CallExpression>\n"
  | If_expression (_,e1,e2,e3) ->
    output_string chan "<IfExpression>\n";
    write_expression e1 chan;
    write_expression e2 chan;
    write_expression e3 chan;
    output_string chan "</IfExpression>\n"
  | Let_expression (_,d,e) ->
    output_string chan "<LetExpression>\n";
    write_declaration d chan;
    write_expression e chan;
    output_string chan "</LetExpression>\n"
  | Unary_expression (_,o,e) ->
    output_string chan "<UnaryExpression>\n";
    write_operator o chan;
    write_expression e chan;
    output_string chan "</UnaryExpression>\n"
  | Binary_expression (_,e1,o,e2) ->
    output_string chan "<BinaryExpression>\n";
    write_expression e1 chan;
    write_operator o chan;
    write_expression e2 chan;
    output_string chan "</BinaryExpression>\n"
  | Array_expression (_,aa) ->
    output_string chan "<ArrayExpression>\n";
    write_array_aggregate aa chan;
    output_string chan "</ArrayExpression>\n"
  | Record_expression (_,ra) ->
    output_string chan "<RecordExpression>\n";
    write_record_aggregate ra chan;
    output_string chan "</RecordExpression>\n"
  | Checked_expression (e,t) ->
    output_string chan ("<CheckedExpression>\n");
    write_expression e chan;
    write_type_denoter t chan;
    output_string chan "</CheckedExpression>\n"

(** Given a aggregate, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and write_array_aggregate t chan =
  match t with    Single_array_aggregate (_,e) ->
    output_string chan "<SingleArrayAggregate>\n";
    write_expression e chan;
    output_string chan "</SingleArrayAggregate>\n"
  | Multiple_array_aggregate (_,e,aa) ->
    output_string chan "<MultipleArrayAggregate>\n";
    write_expression e chan;
    write_array_aggregate aa chan;
    output_string chan "</MultipleArrayAggregate>\n"
  | Checked_array_aggregate (aa,i) ->
    let line =
      ("<CheckedArrayAggregate element_count=\"" ^ string_of_int i ^ "\">\n")
    in
    output_string chan line;
    write_array_aggregate aa chan;
    output_string chan "</CheckedArrayAggregate>\n"

(** Given a record, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and write_record_aggregate t chan =
  match t with    Single_record_aggregate (_,i,e) ->
    output_string chan "<SingleRecordAggregate>\n";
    write_identifier i chan;
    write_expression e chan;
    output_string chan "</SingleRecordAggregate>\n"
  | Multiple_record_aggregate (_,i,e,ra) ->
    output_string chan "<MultipleRecordAggregate>\n";
    write_identifier i chan;
    write_expression e chan;
    write_record_aggregate ra chan;
    output_string chan "</MultipleRecordAggregate>\n"
  | Checked_record_aggregate (ra,t) ->
    output_string chan ("<CheckedRecordAggregate>\n");
    write_record_aggregate ra chan;
    write_field_type_denoter t chan;
    output_string chan "</CheckedRecordAggregate>\n"

(** Given a vname, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and write_vname t chan =
  match t with    Simple_vname (_,i) ->
    output_string chan "<SimpleVname>\n";
    write_identifier i chan;
    output_string chan "</SimpleVname>\n"
  | Dot_vname (_,v,i) ->
    output_string chan "<DotVname>\n";
    write_vname v chan;
    write_identifier i chan;
    output_string chan "</DotVname>\n"
  | Subscript_vname (_,v,e) ->
    output_string chan "<SubscriptVname>\n";
    write_vname v chan;
    write_expression e chan;
    output_string chan "</SubscriptVname>\n"
  | Checked_vname (v,vr,ix,os,t) ->
    let line =
      ("<CheckedVname variable=\"" ^ (string_of_bool vr) ^ "\" indexed=\""
      ^ (string_of_bool ix) ^ "\" offset=\"" ^ (string_of_int os) ^ "\">\n") in
    output_string chan line;
    write_vname v chan;
    write_type_denoter t chan;
    output_string chan "</CheckedVname>\n"

(** Given a declaration, it will run through its tree recursively generating
the openings and closures of tags in the corresponding places.. *)
and write_declaration t chan =
  match t with    Null_declaration -> output_string chan "</NullDeclaration>\n";
  | Const_declaration (_,i,e) ->
    output_string chan "<ConstDeclaration>\n";
    write_identifier i chan;
    write_expression e chan;
    output_string chan "</ConstDeclaration>\n"
  | Var_declaration (_,i,t) ->
    output_string chan "<VarDeclaration>\n";
    write_identifier i chan;
    write_type_denoter t chan;
    output_string chan "</VarDeclaration>\n"
  | Proc_declaration (_,i,fps,c) ->
    output_string chan "<ProcDeclaration>\n";
    write_identifier i chan;
    write_formal_parameter_sequence fps chan;
    write_command c chan;
    output_string chan "</ProcDeclaration>\n"
  | Func_declaration (_,i,fps,t,e) ->
    output_string chan "<FuncDeclaration>\n";
    write_identifier i chan;
    write_formal_parameter_sequence fps chan;
    write_type_denoter t chan;
    write_expression e chan;
    output_string chan "</FuncDeclaration>\n"
  | Type_declaration (_,i,t) ->
    output_string chan "<TypeDeclaration>\n";
    write_identifier i chan;
    write_type_denoter t chan;
    output_string chan "</TypeDeclaration>\n"
  | Unary_operator_declaration (_,o,t1,t2) ->
    output_string chan "<UnaryOperatorDeclaration>\n";
    write_operator o chan;
    write_type_denoter t1 chan;
    write_type_denoter t2 chan;
    output_string chan "</UnaryOperatorDeclaration>\n"
  | Binary_operator_declaration (_,o,t1,t2,t3) ->
    output_string chan "<BinaryOperatorDeclaration>\n";
    write_operator o chan;
    write_type_denoter t1 chan;
    write_type_denoter t2 chan;
    write_type_denoter t3 chan;
    output_string chan "</BinaryOperatorDeclaration>\n"

  | Formal_parameter_declaration (_,fp) ->
    output_string chan "<FormalParameterDeclaration>\n";
    write_formal_parameter fp chan;
    output_string chan "</FormalParameterDeclaration>\n"
  | Sequential_declaration (_,d1,d2) ->
    output_string chan "<SequentialDeclaration>\n";
    write_declaration d1 chan;
    write_declaration d2 chan;
    output_string chan "</SequentialDeclaration>\n"

(** Given a formal parameter, it will run through its tree recursively
generating the openings and closures of tags in the corresponding places.. *)
and write_formal_parameter t chan =
  match t with    Const_formal_parameter (_,i,t) ->
    output_string chan "<ConstFormalParameter>\n";
    write_identifier i chan;
    write_type_denoter t chan;
    output_string chan "</ConstFormalParameter>\n"
  | Var_formal_parameter (_,i,t) ->
    output_string chan "<VarFormalParameter>\n";
    write_identifier i chan;
    write_type_denoter t chan;
    output_string chan "</VarFormalParameter>\n"
  | Proc_formal_parameter (_,i,fps) ->
    output_string chan "<ProcFormalParameter>\n";
    write_identifier i chan;
    write_formal_parameter_sequence fps chan;
    output_string chan "</ProcFormalParameter>\n"
  | Func_formal_parameter (_,i,fps,t) ->
    output_string chan "<FuncFormalParameter>\n";
    write_identifier i chan;
    write_formal_parameter_sequence fps chan;
    write_type_denoter t chan;
    output_string chan "</FuncFormalParameter>\n"

(** Given a actual parameter, it will run through its tree recursively
generating the openings and closures of tags in the corresponding places.. *)
and write_actual_parameter t chan =
  match t with    Const_actual_parameter (_,e) ->
    output_string chan "<ConstActualParameter>\n";
    write_expression e chan;
    output_string chan "</ConstActualParameter>\n"
  | Var_actual_parameter (_,v) ->
    output_string chan "<VarActualParameter>\n";
    write_vname v chan;
    output_string chan "</VarActualParameter>\n"
  | Proc_actual_parameter (_,i) ->
    output_string chan "<ProcActualParameter>\n";
    write_identifier i chan;
    output_string chan "</ProcActualParameter>\n"
  | Func_actual_parameter (_,i) ->
    output_string chan "<FuncActualParameter>\n";
    write_identifier i chan;
    output_string chan "</FuncActualParameter>\n"

(** Given a formal parameter sequence, it will run through its tree recursively
generating the openings and closures of tags in the corresponding places.. *)
and write_formal_parameter_sequence t chan =
  match t with    Empty_formal_parameter_sequence (_) ->
    output_string chan "<EmptyFormalParameterSequence/>\n"
  | Single_formal_parameter_sequence (_,fp) ->
    output_string chan "<SingleFormalParameterSequence>\n";
    write_formal_parameter fp chan;
    output_string chan "</SingleFormalParameterSequence>\n"
  | Multiple_formal_parameter_sequence (_,fp,fps) ->
    output_string chan "<MultipleFormalParameterSequence>\n";
    write_formal_parameter fp chan;
    write_formal_parameter_sequence fps chan;
    output_string chan "</MultipleFormalParameterSequence>\n"

(** Given a actual parameter sequence, it will run through its tree recursively
generating the openings and closures of tags in the corresponding places.. *)
and write_actual_parameter_sequence t chan =
  match t with    Empty_actual_parameter_sequence (_) ->
    output_string chan "<EmptyActualParameterSequence/>\n"
  | Single_actual_parameter_sequence (_,ap) ->
    output_string chan "<SingleActualParameterSequence>\n";
    write_actual_parameter ap chan;
    output_string chan "</SingleActualParameterSequence>\n"
  | Multiple_actual_parameter_sequence (_,ap,aps) ->
    output_string chan "<MultipleActualParameterSequence>\n";
    write_actual_parameter ap chan;
    write_actual_parameter_sequence aps chan;
    output_string chan "</MultipleActualParameterSequence>\n"

(** Given a type denoter, it will run through its tree recursively generating
the openings and closures of tags in the corresponding places.. *)
and write_type_denoter t chan =
  match t with    Null_type_denoter -> output_string chan "<NullTypeDenoter/>\n"
  | Error_type_denoter (_) ->
    output_string chan "<ErrorTypeDenoter/>\n"
  | Any_type_denoter (_) ->
    output_string chan "<AnyTypeDenoter/>\n"
  | Simple_type_denoter (_,i) ->
    output_string chan "<SimpleTypeDenoter>\n";
    write_identifier i chan;
    output_string chan "</SimpleTypeDenoter>\n"
  | Array_type_denoter (_,il,t) ->
    output_string chan "<ArrayTypeDenoter>\n";
    write_integer_literal il chan;
    write_type_denoter t chan;
    output_string chan "</ArrayTypeDenoter>\n"
  | Record_type_denoter (_,ft) ->
    output_string chan "<RecordTypeDenoter>\n";
    write_field_type_denoter ft chan;
    output_string chan "</RecordTypeDenoter>\n"
  | Bool_type_denoter (_) ->
    output_string chan "<BoolTypeDenoter/>\n"
  | Int_type_denoter (_) ->
    output_string chan "<IntTypeDenoter/>\n"
  | Char_type_denoter (_) ->
    output_string chan "<CharTypeDenoter/>\n"

(** Given a field type denoter, it will run through its tree recursively
generating the openings and closures of tags in the corresponding places.. *)
and write_field_type_denoter t chan =
  match t with    Single_field_type_denoter (_,i,t) ->
    output_string chan "<SingleFieldTypeDenoter>\n";
    write_identifier i chan;
    write_type_denoter t chan;
    output_string chan "</SingleFieldTypeDenoter>\n"
  | Multiple_field_type_denoter (_,i,t,ft) ->
    output_string chan "<MultipleFieldTypeDenoter>\n";
    write_identifier i chan;
    write_type_denoter t chan;
    write_field_type_denoter ft chan;
    output_string chan "</MultipleFieldTypeDenoter>\n"

(** Given a integer literal this prints the XML version *)
and write_integer_literal t chan =
  match t with    Integer_literal (_,str) ->
    output_string chan ("<IntegerLiteral value=\"" ^ str ^ "\"/>\n")

(** Given a character literal this prints the XML version *)
and write_character_literal t chan =
  match t with
  Character_literal (_, str) ->
    let line =
      ("<CharacterLiteral value=\"" ^ (clean_String str) ^ "\"/>\n") in
    output_string chan line

(** Given a identifier this prints the XML version *)
and write_identifier t chan =
  match t with    Identifier (_,str) ->
    output_string chan ("<Identifier value=\"" ^ str ^ "\"/>\n")
  | Checked_identifier (i,d) ->
    output_string chan ("<CheckedIdentifier>\n");
    write_identifier i chan;
    output_string chan "</CheckedIdentifier>\n"

(** Given a operator this prints the XML version *)
and write_operator t chan =
  match t with    Operator (_,str) ->
    output_string chan ("<Operator value=\"" ^ (clean_String str) ^ "\"/>\n")
  | Checked_operator (o,d) ->
    output_string chan "<CheckedOperator>\n";
    write_operator o chan;
    output_string chan "</CheckedOperator>\n"

(** This function receives the AST to be printed and the name of the file
in which it will be written). It is responsible for creating the file *)
let write_x_m_l_tree astree fname =
  try
    let chan = open_out fname in
  	output_string chan "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
    write_program astree chan;
    close_out_noerr chan
  with
    Sys_error s -> printf "Couldn't write XML tree file. (%s)\n" s

(** Note: The first argument of all printing functions is ignored because it
stores irrelevant information for printing. *)