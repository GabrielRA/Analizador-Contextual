(**
This program was made by Luis Leopoldo Pérez at March 14, 2006.
This program was repaired, completed, verified and validated by students of
ITCR at 2018.
Abstract Syntax Tree Definition for Caml-Triangle
Interface file                                    

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)


open RuntimeEntity


(** Type to store the information referring to a part of the AST *)
type ast_info = {pos: Lexing.position; mutable run: runtime_entity}

(** Type to store the abstract data structure of the program *)
type ast_program =
    Null_program
  | Program of ast_info * ast_command

(** Definition of the possible structure of a command *)
and ast_command =
    Empty_command of ast_info
  | Assign_command of ast_info * ast_vname * ast_expression
  | Call_command of ast_info * ast_identifier * ast_actual_parameter_sequence
  | Sequential_command of ast_info * ast_command * ast_command
  | Let_command of ast_info * ast_declaration * ast_command
  | If_command of ast_info * ast_expression * ast_command * ast_command
  | While_command of ast_info * ast_expression * ast_command

(** Definition of the possible structure of an expression *)
and ast_expression =
    Empty_expression of ast_info
  | Integer_expression of ast_info * ast_integer_literal
  | Character_expression of ast_info * ast_character_literal
  | Vname_expression of ast_info * ast_vname
  | Call_expression of ast_info * ast_identifier * ast_actual_parameter_sequence
  | If_expression of ast_info * ast_expression * ast_expression * ast_expression
  | Let_expression of ast_info * ast_declaration * ast_expression
  | Unary_expression of ast_info * ast_operator * ast_expression
  | Binary_expression of ast_info * ast_expression * ast_operator
  * ast_expression
  | Array_expression of ast_info * ast_array_aggregate
  | Record_expression of ast_info * ast_record_aggregate
  | Checked_expression of ast_expression * ast_type_denoter

(** Definition of the possible structure of an array agregate *)
and ast_array_aggregate =
    Single_array_aggregate of ast_info * ast_expression
  | Multiple_array_aggregate of ast_info * ast_expression * ast_array_aggregate
  | Checked_array_aggregate of ast_array_aggregate * int

(** Definition of the possible structure of a record agregate *)
and ast_record_aggregate =
    Single_record_aggregate of ast_info * ast_identifier * ast_expression
  | Multiple_record_aggregate of ast_info * ast_identifier * ast_expression
  * ast_record_aggregate
  | Checked_record_aggregate of ast_record_aggregate * ast_field_type_denoter

(** Definition of the possible structure of a vname *)
and ast_vname =
    Simple_vname of ast_info * ast_identifier
  | Dot_vname of ast_info * ast_vname * ast_identifier
  | Subscript_vname of ast_info * ast_vname * ast_expression
  | Checked_vname of ast_vname * bool * bool * int * ast_type_denoter

(** Definition of the possible structure of a declaration *)
and ast_declaration =
    Null_declaration
  | Const_declaration of ast_info * ast_identifier * ast_expression
  | Var_declaration of ast_info * ast_identifier * ast_type_denoter
  | Proc_declaration of ast_info * ast_identifier
  * ast_formal_parameter_sequence * ast_command
  | Func_declaration of ast_info * ast_identifier
  * ast_formal_parameter_sequence * ast_type_denoter * ast_expression
  | Type_declaration of ast_info * ast_identifier * ast_type_denoter
  | Unary_operator_declaration of ast_info * ast_operator * ast_type_denoter
  * ast_type_denoter
  | Binary_operator_declaration of ast_info * ast_operator * ast_type_denoter
  * ast_type_denoter * ast_type_denoter
  | Formal_parameter_declaration of ast_info * ast_formal_parameter
  | Sequential_declaration of ast_info * ast_declaration * ast_declaration

(** Definition of the possible structure of a formal parameter *)
and ast_formal_parameter =
    Const_formal_parameter of ast_info * ast_identifier * ast_type_denoter
  | Var_formal_parameter of ast_info * ast_identifier * ast_type_denoter
  | Proc_formal_parameter of ast_info * ast_identifier
  * ast_formal_parameter_sequence
  | Func_formal_parameter of ast_info * ast_identifier
  * ast_formal_parameter_sequence * ast_type_denoter

(** Definition of the possible structure of a actual parameter *)
and ast_actual_parameter =
    Const_actual_parameter of ast_info * ast_expression
  | Var_actual_parameter of ast_info * ast_vname
  | Proc_actual_parameter of ast_info * ast_identifier
  | Func_actual_parameter of ast_info * ast_identifier

(** Definition of the possible structure of a formal paramater sequence *)
and ast_formal_parameter_sequence =
    Empty_formal_parameter_sequence of ast_info
  | Single_formal_parameter_sequence of ast_info * ast_formal_parameter
  | Multiple_formal_parameter_sequence of ast_info * ast_formal_parameter
  * ast_formal_parameter_sequence

(** Definition of the possible structure of an actual parameter sequence *)
and ast_actual_parameter_sequence =
    Empty_actual_parameter_sequence of ast_info
  | Single_actual_parameter_sequence of ast_info * ast_actual_parameter
  | Multiple_actual_parameter_sequence of ast_info * ast_actual_parameter
  * ast_actual_parameter_sequence

(** Definition of the possible structure of a type denoter *)
and ast_type_denoter =
    Null_type_denoter
  | Error_type_denoter of ast_info
  | Any_type_denoter of ast_info
  | Simple_type_denoter of ast_info * ast_identifier
  | Array_type_denoter of ast_info * ast_integer_literal * ast_type_denoter
  | Record_type_denoter of ast_info * ast_field_type_denoter
  | Bool_type_denoter of ast_info
  | Int_type_denoter of ast_info
  | Char_type_denoter of ast_info

(** Definition of the possible structure of a field type denoter *)
and ast_field_type_denoter =
    Single_field_type_denoter of ast_info * ast_identifier * ast_type_denoter
  | Multiple_field_type_denoter of ast_info * ast_identifier * ast_type_denoter
  * ast_field_type_denoter

(** Definition of the possible structure of an integer literal *)
and ast_integer_literal =
    Integer_literal of ast_info * string

(** Definition of the possible structure of an caracter literal *)
and ast_character_literal =
    Character_literal of ast_info * string

(** Definition of the possible structure of an identifier *)
and ast_identifier =
    Identifier of ast_info * string
  | Checked_identifier of ast_identifier * ast_declaration ref

(** Definition of the possible structure of an operator *)
and ast_operator =
    Operator of ast_info * string
  | Checked_operator of ast_operator * ast_declaration ref

(*
Note: Note: Some structures in the abstract syntax tree exist in order to
lighten the work of the contextual analizer
*)