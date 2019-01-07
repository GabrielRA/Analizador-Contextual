(* ------------------------------------------------- *)
(* Abstract Syntax Tree Definition for Caml-Triangle *)
(* Interface file                                    *)
(*                                                   *)
(* (c) 2006 Luis Leopoldo Pérez.                     *)
(* Last modification: March 14, 2006                 *)
(* ------------------------------------------------- *)

open RuntimeEntity

type astInfo = {pos: Lexing.position; mutable run: runtimeEntity}

type astProgram                = Null_program
                               | Program                         of astInfo * astCommand

and astCommand                 = Empty_command                    of astInfo
                               | Assign_command                   of astInfo * astVname * astExpression
                               | Call_command                     of astInfo * astIdentifier * astActualParameterSequence
                               | Sequential_command               of astInfo * astCommand * astCommand
                               | LetCommand                      of astInfo * astDeclaration * astCommand
                               | IfCommand                       of astInfo * astExpression * astCommand * astCommand
                               | While_command                    of astInfo * astExpression * astCommand

and astExpression              = Empty_expression                 of astInfo
                               | Integer_expression               of astInfo * astInteger_literal
                               | Character_expression             of astInfo * astCharacter_literal
                               | Vname_expression                 of astInfo * astVname
                               | Call_expression                  of astInfo * astIdentifier * astActualParameterSequence
                               | If_expression                    of astInfo * astExpression * astExpression * astExpression
                               | Let_expression                   of astInfo * astDeclaration * astExpression
                               | Unary_expression                 of astInfo * astOperator * astExpression
                               | Binary_expression                of astInfo * astExpression * astOperator * astExpression
                               | Array_expression                 of astInfo * astArrayAggregate
                               | Record_expression                of astInfo * astRecordAggregate
                               | Checked_expression               of astExpression * astTypeDenoter

and astArrayAggregate          = Single_array_aggregate            of astInfo * astExpression
                               | Multiple_array_aggregate          of astInfo * astExpression * astArrayAggregate
                               | CheckedArrayAggregate           of astArrayAggregate * int

and astRecordAggregate         = SingleRecordAggregate           of astInfo * astIdentifier * astExpression
                               | MultipleRecordAggregate         of astInfo * astIdentifier * astExpression * astRecordAggregate
                               | Checked_record_aggregate          of astRecordAggregate * astFieldTypeDenoter

and astVname                   = Simple_vname                     of astInfo * astIdentifier
                               | Dot_vname                        of astInfo * astVname * astIdentifier
                               | Subscript_vname                  of astInfo * astVname * astExpression
                               | Checked_vname                    of astVname * bool * bool * int * astTypeDenoter
                                                                               (* variable, indexed, offset *)

and astDeclaration             = Null_declaration
			       | Const_declaration                of astInfo * astIdentifier * astExpression
                               | Var_declaration                  of astInfo * astIdentifier * astTypeDenoter
                               | Proc_declaration                 of astInfo * astIdentifier * astFormalParameterSequence * astCommand
                               | Func_declaration                 of astInfo * astIdentifier * astFormalParameterSequence * astTypeDenoter * astExpression
                               | Type_declaration                 of astInfo * astIdentifier * astTypeDenoter
                               | Unary_operator_declaration        of astInfo * astOperator * astTypeDenoter * astTypeDenoter
                               | Binary_operator_declaration       of astInfo * astOperator * astTypeDenoter * astTypeDenoter * astTypeDenoter
                               | Formal_parameter_declaration      of astInfo * astFormalParameter
                               | Sequential_declaration           of astInfo * astDeclaration * astDeclaration

and astFormalParameter         = Const_formal_parameter            of astInfo * astIdentifier * astTypeDenoter
                               | Var_formal_parameter              of astInfo * astIdentifier * astTypeDenoter
                               | Proc_formal_parameter             of astInfo * astIdentifier * astFormalParameterSequence
                               | Func_formal_parameter             of astInfo * astIdentifier * astFormalParameterSequence * astTypeDenoter

and astActualParameter         = Const_actual_parameter            of astInfo * astExpression
                               | Var_actual_parameter              of astInfo * astVname
                               | Proc_actual_parameter             of astInfo * astIdentifier
                               | Func_actual_parameter             of astInfo * astIdentifier

and astFormalParameterSequence = Empty_formal_parameter_sequence    of astInfo
                               | Single_formal_parameter_sequence   of astInfo * astFormalParameter
                               | Multiple_formal_parameter_sequence of astInfo * astFormalParameter * astFormalParameterSequence

and astActualParameterSequence = EmptyActualParameterSequence    of astInfo
                               | SingleActualParameterSequence   of astInfo * astActualParameter
                               | MultipleActualParameterSequence of astInfo * astActualParameter * astActualParameterSequence

and astTypeDenoter             = Null_type_denoter
                               | Error_type_denoter                of astInfo
                               | Any_type_denoter                  of astInfo
                               | SimpleTypeDenoter               of astInfo * astIdentifier
                               | Array_type_denoter                of astInfo * astInteger_literal * astTypeDenoter
                               | Record_type_denoter               of astInfo * astFieldTypeDenoter
                               | Bool_type_denoter                 of astInfo
                               | Int_type_denoter                  of astInfo
                               | Char_type_denoter                 of astInfo

and astFieldTypeDenoter        = Single_field_type_denoter          of astInfo * astIdentifier * astTypeDenoter
                               | Multiple_field_type_denoter        of astInfo * astIdentifier * astTypeDenoter * astFieldTypeDenoter

and astInteger_literal          = Integer_literal                  of astInfo * string

and astCharacter_literal        = Character_literal                of astInfo * string

and astIdentifier              = Identifier                      of astInfo * string
                               | Checked_identifier               of astIdentifier * astDeclaration ref

and astOperator                = Operator                        of astInfo * string
                               | Checked_operator                 of astOperator * astDeclaration ref

