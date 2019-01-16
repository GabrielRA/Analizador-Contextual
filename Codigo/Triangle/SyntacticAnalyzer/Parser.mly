%{

(* --------------------------------------------------- *)
(* Parser generator file (ocamlyacc) for Caml-Triangle *)
(*                                                     *)
(* (c) 2006 Luis Leopoldo Pérez.                       *)
(* Last modification: April 12, 2006                   *)
(* --------------------------------------------------- *)

      open Ast
      open Printf
      open Parsing
      open ErrorReporter
      open RuntimeEntity
      
      let parse_error s = ()
%}


/* Token definitions */

%token <string> INTLITERAL
%token <string> CHARLITERAL
%token <string> IDENTIFIER
%token <string> OPERATOR
%token ARRAY BEGIN CONST DO ELSE END FUNC IF IN LET OF PROC RECORD THEN TYPE VAR WHILE
%token DOT COLON SEMICOLON COMMA BECOMES IS LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY
%token EOF

/* Start rule and return type definition */

%start parseProgram
%type <Ast.ast_program> parseProgram

%%

/* Rules - every action returns an abstract syntax tree for the newly recognized rule. */

/* Programs (starting rule) */
parseProgram: Command EOF   { Program({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
            | error         { ErrorReporter.report_error "Command expected here." (rhs_start_pos(1)); 
                              raise Parse_error }
            ;


/* Commands */
Command: single_Command                   { $1 }
       | single_Command SEMICOLON Command { Sequential_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
       ;

single_Command:                                                       { Empty_command({pos=rhs_start_pos(1);run=Null_runtime_entity}) }
              | Vname BECOMES Expression                              { Assign_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
              | Identifier LPAREN Actual_Parameter_Sequence RPAREN    { Call_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
              | BEGIN Command END                                     { $2 }
              | LET Declaration IN single_Command                     { Let_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
              | IF Expression THEN single_Command ELSE single_Command { If_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $6) }
              | WHILE Expression DO single_Command                    { While_command({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }

              ;

/* Expressions */
Expression: secondary_Expression                             { $1 }
          | LET Declaration IN Expression                    { Let_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
          | IF Expression THEN Expression ELSE Expression    { If_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $6) }
          | error                                            { ErrorReporter.report_error "Expression expected here." (rhs_start_pos(1)); 
                                                               raise Parse_error }
          ;


secondary_Expression: primary_Expression                               { $1 }
                    | secondary_Expression Operator primary_Expression { Binary_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $2, $3) }
                    ;

primary_Expression: Integer_Literal                                    { Integer_expression({pos=rhs_start_pos(1); run=Null_runtime_entity}, $1) }
                  | Character_Literal                                  { Character_expression({pos=rhs_start_pos(1);run=Null_runtime_entity},$1) }
                  | Vname                                              { Vname_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                  | Identifier LPAREN Actual_Parameter_Sequence RPAREN { Call_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                  | Operator primary_Expression                        { Unary_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $2) }
                  | LPAREN Expression RPAREN                           { $2 }
                  | LCURLY Record_Aggregate RCURLY                     { Record_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                  | LBRACKET Array_Aggregate RBRACKET                  { Array_expression({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                  ;

/* Record Aggregate Expressions */
Record_Aggregate: Identifier IS Expression                        { Single_record_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                | Identifier IS Expression COMMA Record_Aggregate { Multiple_record_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3, $5) }
                ;

/* Array Aggregate Expressions */
Array_Aggregate: Expression                       { Single_array_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
               | Expression COMMA Array_Aggregate { Multiple_array_aggregate({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
               ;

/* Value-or-variable names */
Vname: Identifier                         { Simple_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
     | Vname DOT Identifier               { Dot_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
     | Vname LBRACKET Expression RBRACKET { Subscript_vname({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
     ;

/* Declarations */
Declaration: single_Declaration                       { $1 }
           | Declaration SEMICOLON single_Declaration { Sequential_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
           | error                                    { ErrorReporter.report_error "Declaration expected here." (rhs_start_pos(1)); 
                                                        raise Parse_error }
           ;

single_Declaration: CONST Identifier IS Expression                                                           { Const_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                  | VAR Identifier COLON Type_denoter                                                        { Var_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                  | PROC Identifier LPAREN Formal_Parameter_Sequence RPAREN IS single_Command                { Proc_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $7) }
                  | FUNC Identifier LPAREN Formal_Parameter_Sequence RPAREN COLON Type_denoter IS Expression { Func_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $7, $9) }
                  | TYPE Identifier IS Type_denoter                                                          { Type_declaration({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                  ;

/* Formal Parameters */
Formal_Parameter_Sequence:                                  { Empty_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}) }
                         | proper_Formal_Parameter_Sequence { $1 }
                         ;

proper_Formal_Parameter_Sequence: Formal_Parameter                                        { Single_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity},$1) }
                                | Formal_Parameter COMMA proper_Formal_Parameter_Sequence { Multiple_formal_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                                ;

Formal_Parameter: Identifier COLON Type_denoter                                              { Const_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                | VAR Identifier COLON Type_denoter                                          { Var_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                | PROC Identifier LPAREN Formal_Parameter_Sequence RPAREN                    { Proc_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
                | FUNC Identifier LPAREN Formal_Parameter_Sequence RPAREN COLON Type_denoter { Func_formal_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4, $7) }
                ;

/* Actual Parameters */
Actual_Parameter_Sequence:                                  { Empty_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}) }
                         | proper_Actual_Parameter_Sequence { $1 }
                         ;

proper_Actual_Parameter_Sequence: Actual_Parameter                                        { Single_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                                | Actual_Parameter COMMA proper_Actual_Parameter_Sequence { Multiple_actual_parameter_sequence({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                                ;

Actual_Parameter: Expression      { Const_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                | VAR Vname       { Var_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                | PROC Identifier { Proc_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                | FUNC Identifier { Func_actual_parameter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
                ;

/* Type denoters */
Type_denoter: Identifier                            { Simple_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
            | ARRAY Integer_Literal OF Type_denoter { Array_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2, $4) }
            | RECORD Record_Type_denoter END        { Record_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $2) }
            ;

Record_Type_denoter: Identifier COLON Type_denoter                           { Single_field_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3) }
                   | Identifier COLON Type_denoter COMMA Record_Type_denoter { Multiple_field_type_denoter({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1, $3, $5) }
                   ;

/* Integer Literals */
Integer_Literal: INTLITERAL { Integer_literal({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
               ;

/* Character Literals */
Character_Literal: CHARLITERAL { Character_literal({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
                 ;

/* Identifiers */
Identifier: IDENTIFIER { Identifier({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
          ;

/* Operators */
Operator: OPERATOR { Operator({pos=rhs_start_pos(1);run=Null_runtime_entity}, $1) }
        ;

%%