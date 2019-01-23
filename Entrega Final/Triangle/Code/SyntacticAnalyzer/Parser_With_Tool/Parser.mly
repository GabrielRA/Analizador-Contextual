%{

(**
This program was originally written by Luis Leopoldo Pérez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by students of ITCR in January 2019.
LParser generator file (ocamlyacc) for Caml-Triangle                                    

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)



open Ast
open Parsing
open ErrorReporter
open RuntimeEntity
open Token

(** This is the declaration of exception of program *)
exception Synt_error of string;;

let parse_error s = ()
%}


/* Token definitions */

%token <string> Int_literal
%token <string> Char_literal
%token <string> Identifier
%token <string> Operator
%token Array Begin Const Do Else End Func If In Let Of Proc Record Then Type
%token Var While Dot Colon Semicolon Comma Becomes Is Lparen Rparen Lbracket
%token Rbracket Lcurly Rcurly
%token Eof

/* Start rule and return type definition */

%start parse_program
%type <Ast.ast_program> parse_program

%%

/*  Rules - every action returns an abstract syntax tree for the newly
    recognized rule. */

/* Programs (starting rule) */
parse_program: Command Eof   { Program({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
             ;


/* Commands */
Command: single_Command                   { $1 }
       | Command Semicolon single_Command { Sequential_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
       ;

single_Command:                                                       { Empty_command({pos=rhs_start_pos(1);run=Null_runtime_entity}) }
              | Vname Becomes Expression                              { Assign_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
              | R_Identifier Lparen Actual_Parameter_Sequence Rparen    { Call_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
              | Begin Command End                                     { $2 }
              | Let Declaration In single_Command                     { Let_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
              | If Expression Then single_Command Else single_Command { If_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $6) }
              | While Expression Do single_Command                    { While_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
              | error                                                 { ErrorReporter.report_error "Cannot start a command." (rhs_start_pos(1)); 
                                                                        raise (Synt_error "Syntactical error") }
              ;

/* Expressions */
Expression: secondary_Expression                             { $1 }
          | Let Declaration In Expression                    { Let_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
          | If Expression Then Expression Else Expression    { If_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $6) }
          | error                                            { ErrorReporter.report_error "Cannot start a expression." (rhs_start_pos(1)); 
                                                               raise (Synt_error "Syntactical error") }
          ;


secondary_Expression: primary_Expression                               { $1 }
                    | secondary_Expression R_Operator primary_Expression { Binary_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $2, $3) }
                    ;

primary_Expression: Integer_Literal                                    { Integer_expression({pos=rhs_start_pos(1); run=Null_runtime_entity}, $1) }
                  | Character_Literal                                  { Character_expression({pos=rhs_start_pos(1);run=Null_runtime_entity},$1) }
                  | Vname                                              { Vname_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                  | R_Identifier Lparen Actual_Parameter_Sequence Rparen { Call_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                  | R_Operator primary_Expression                        { Unary_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $2) }
                  | Lparen Expression Rparen                           { $2 }
                  | Lcurly Record_Aggregate Rcurly                     { Record_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                  | Lbracket Array_Aggregate Rbracket                  { Array_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                  ;

/* Record Aggregate Expressions */
Record_Aggregate: R_Identifier Is Expression                        { Single_record_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                | R_Identifier Is Expression Comma Record_Aggregate { Multiple_record_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3, $5) }
                ;

/* Array Aggregate Expressions */
Array_Aggregate: Expression                       { Single_array_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
               | Expression Comma Array_Aggregate { Multiple_array_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
               ;

/* Value-or-variable names */
Vname: R_Identifier                         { Simple_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
     | Vname Dot R_Identifier               { Dot_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
     | Vname Lbracket Expression Rbracket { Subscript_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
     ;

/* Declarations */
Declaration: single_Declaration                       { $1 }
           | Declaration Semicolon single_Declaration { Sequential_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
           | error                                    { ErrorReporter.report_error "Cannot start a declaration." (rhs_start_pos(1)); 
                                                        raise (Synt_error "Syntactical error") }
           ;

single_Declaration: Const R_Identifier Is Expression                                                           { Const_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                  | Var R_Identifier Colon Type_denoter                                                        { Var_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                  | Proc R_Identifier Lparen Formal_Parameter_Sequence Rparen Is single_Command                { Proc_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $7) }
                  | Func R_Identifier Lparen Formal_Parameter_Sequence Rparen Colon Type_denoter Is Expression { Func_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $7, $9) }
                  | Type R_Identifier Is Type_denoter                                                          { Type_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                  ;

/* Formal Parameters */
Formal_Parameter_Sequence:                                  { Empty_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}) }
                         | proper_Formal_Parameter_Sequence { $1 }
                         ;

proper_Formal_Parameter_Sequence: Formal_Parameter                                        { Single_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity},$1) }
                                | Formal_Parameter Comma proper_Formal_Parameter_Sequence { Multiple_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                                ;

Formal_Parameter: R_Identifier Colon Type_denoter                                              { Const_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                | Var R_Identifier Colon Type_denoter                                          { Var_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                | Proc R_Identifier Lparen Formal_Parameter_Sequence Rparen                    { Proc_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                | Func R_Identifier Lparen Formal_Parameter_Sequence Rparen Colon Type_denoter { Func_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $7) }
                | error                                                                        { ErrorReporter.report_error "Cannot start a formal parameter." (rhs_start_pos(1)); 
                                                                                                 raise (Synt_error "Syntactical error") }
                ;

/* Actual Parameters */
Actual_Parameter_Sequence:                                  { Empty_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}) }
                         | proper_Actual_Parameter_Sequence { $1 }
                         ;

proper_Actual_Parameter_Sequence: Actual_Parameter                                        { Single_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                                | Actual_Parameter Comma proper_Actual_Parameter_Sequence { Multiple_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                                ;

Actual_Parameter: Expression        { Const_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                | Var Vname         { Var_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                | Proc R_Identifier { Proc_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                | Func R_Identifier { Func_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                ; 

/* Type denoters */
Type_denoter: R_Identifier                          { Simple_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
            | Array Integer_Literal Of Type_denoter { Array_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
            | Record Record_Type_denoter End        { Record_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
            | error                                 { ErrorReporter.report_error "Cannot start type denoter." (rhs_start_pos(1)); 
                                                      raise (Synt_error "Syntactical error") };

Record_Type_denoter: R_Identifier Colon Type_denoter                           { Single_field_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                   | R_Identifier Colon Type_denoter Comma Record_Type_denoter { Multiple_field_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3, $5) }
                   ;

/* Integer Literals */
Integer_Literal: Int_literal { Integer_literal({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
               ;

/* Character Literals */
Character_Literal: Char_literal { Character_literal({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                 ;

/* R_Identifiers */
R_Identifier: Identifier { Ast.Identifier({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
          ;

/* R_Operators */
R_Operator: Operator { Ast.Operator({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
        ;

%%