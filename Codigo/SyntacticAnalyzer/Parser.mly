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
%type <Ast.astProgram> parseProgram

%%

/* Rules - every action returns an abstract syntax tree for the newly recognized rule. */

/* Programs (starting rule) */
parseProgram: Command EOF   { Program({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
            | error         { ErrorReporter.reportError "Command expected here." (rhs_start_pos(1)); 
                              raise Parse_error }
            ;


/* Commands */
Command: single_Command                   { $1 }
       | single_Command SEMICOLON Command { SequentialCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
       ;

single_Command:                                                       { EmptyCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}) }
              | Vname BECOMES Expression                              { AssignCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
              | Identifier LPAREN Actual_Parameter_Sequence RPAREN    { CallCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
              | BEGIN Command END                                     { $2 }
              | LET Declaration IN single_Command                     { LetCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
              | IF Expression THEN single_Command ELSE single_Command { IfCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4, $6) }
              | WHILE Expression DO single_Command                    { WhileCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }

              ;

/* Expressions */
Expression: secondary_Expression                             { $1 }
          | LET Declaration IN Expression                    { LetExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
          | IF Expression THEN Expression ELSE Expression    { IfExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4, $6) }
          | error                                            { ErrorReporter.reportError "Expression expected here." (rhs_start_pos(1)); 
                                                               raise Parse_error }
          ;


secondary_Expression: primary_Expression                               { $1 }
                    | secondary_Expression Operator primary_Expression { BinaryExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $2, $3) }
                    ;

primary_Expression: Integer_Literal                                    { IntegerExpression({pos=rhs_start_pos(1); run=NullRuntimeEntity}, $1) }
                  | Character_Literal                                  { CharacterExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity},$1) }
                  | Vname                                              { VnameExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
                  | Identifier LPAREN Actual_Parameter_Sequence RPAREN { CallExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
                  | Operator primary_Expression                        { UnaryExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $2) }
                  | LPAREN Expression RPAREN                           { $2 }
                  | LCURLY Record_Aggregate RCURLY                     { RecordExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2) }
                  | LBRACKET Array_Aggregate RBRACKET                  { ArrayExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2) }
                  ;

/* Record Aggregate Expressions */
Record_Aggregate: Identifier IS Expression                        { SingleRecordAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
                | Identifier IS Expression COMMA Record_Aggregate { MultipleRecordAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3, $5) }
                ;

/* Array Aggregate Expressions */
Array_Aggregate: Expression                       { SingleArrayAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
               | Expression COMMA Array_Aggregate { MultipleArrayAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
               ;

/* Value-or-variable names */
Vname: Identifier                         { SimpleVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
     | Vname DOT Identifier               { DotVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
     | Vname LBRACKET Expression RBRACKET { SubscriptVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
     ;

/* Declarations */
Declaration: single_Declaration                       { $1 }
           | Declaration SEMICOLON single_Declaration { SequentialDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
           | error                                    { ErrorReporter.reportError "Declaration expected here." (rhs_start_pos(1)); 
                                                        raise Parse_error }
           ;

single_Declaration: CONST Identifier IS Expression                                                           { ConstDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
                  | VAR Identifier COLON Type_denoter                                                        { VarDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
                  | PROC Identifier LPAREN Formal_Parameter_Sequence RPAREN IS single_Command                { ProcDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4, $7) }
                  | FUNC Identifier LPAREN Formal_Parameter_Sequence RPAREN COLON Type_denoter IS Expression { FuncDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4, $7, $9) }
                  | TYPE Identifier IS Type_denoter                                                          { TypeDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
                  ;

/* Formal Parameters */
Formal_Parameter_Sequence:                                  { EmptyFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}) }
                         | proper_Formal_Parameter_Sequence { $1 }
                         ;

proper_Formal_Parameter_Sequence: Formal_Parameter                                        { SingleFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity},$1) }
                                | Formal_Parameter COMMA proper_Formal_Parameter_Sequence { MultipleFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
                                ;

Formal_Parameter: Identifier COLON Type_denoter                                              { ConstFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
                | VAR Identifier COLON Type_denoter                                          { VarFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
                | PROC Identifier LPAREN Formal_Parameter_Sequence RPAREN                    { ProcFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
                | FUNC Identifier LPAREN Formal_Parameter_Sequence RPAREN COLON Type_denoter { FuncFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4, $7) }
                ;

/* Actual Parameters */
Actual_Parameter_Sequence:                                  { EmptyActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}) }
                         | proper_Actual_Parameter_Sequence { $1 }
                         ;

proper_Actual_Parameter_Sequence: Actual_Parameter                                        { SingleActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
                                | Actual_Parameter COMMA proper_Actual_Parameter_Sequence { MultipleActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
                                ;

Actual_Parameter: Expression      { ConstActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
                | VAR Vname       { VarActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2) }
                | PROC Identifier { ProcActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2) }
                | FUNC Identifier { FuncActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2) }
                ;

/* Type denoters */
Type_denoter: Identifier                            { SimpleTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
            | ARRAY Integer_Literal OF Type_denoter { ArrayTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2, $4) }
            | RECORD Record_Type_denoter END        { RecordTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $2) }
            ;

Record_Type_denoter: Identifier COLON Type_denoter                           { SingleFieldTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3) }
                   | Identifier COLON Type_denoter COMMA Record_Type_denoter { MultipleFieldTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1, $3, $5) }
                   ;

/* Integer Literals */
Integer_Literal: INTLITERAL { IntegerLiteral({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
               ;

/* Character Literals */
Character_Literal: CHARLITERAL { CharacterLiteral({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
                 ;

/* Identifiers */
Identifier: IDENTIFIER { Identifier({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
          ;

/* Operators */
Operator: OPERATOR { Operator({pos=rhs_start_pos(1);run=NullRuntimeEntity}, $1) }
        ;

%%