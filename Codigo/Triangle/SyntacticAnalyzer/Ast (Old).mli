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

and astCommand                 = EmptyCommand                    of astInfo
                               | AssignCommand                   of astInfo * astVname * astExpression
                               | CallCommand                     of astInfo * astIdentifier * astActualParameterSequence
                               | SequentialCommand               of astInfo * astCommand * astCommand
                               | LetCommand                      of astInfo * astDeclaration * astCommand
                               | IfCommand                       of astInfo * astExpression * astCommand * astCommand
                               | WhileCommand                    of astInfo * astExpression * astCommand

and astExpression              = EmptyExpression                 of astInfo
                               | IntegerExpression               of astInfo * astIntegerLiteral
                               | CharacterExpression             of astInfo * astCharacterLiteral
                               | VnameExpression                 of astInfo * astVname
                               | CallExpression                  of astInfo * astIdentifier * astActualParameterSequence
                               | IfExpression                    of astInfo * astExpression * astExpression * astExpression
                               | LetExpression                   of astInfo * astDeclaration * astExpression
                               | UnaryExpression                 of astInfo * astOperator * astExpression
                               | BinaryExpression                of astInfo * astExpression * astOperator * astExpression
                               | ArrayExpression                 of astInfo * astArrayAggregate
                               | RecordExpression                of astInfo * astRecordAggregate
                               | CheckedExpression               of astExpression * astTypeDenoter

and astArrayAggregate          = SingleArrayAggregate            of astInfo * astExpression
                               | MultipleArrayAggregate          of astInfo * astExpression * astArrayAggregate
                               | CheckedArrayAggregate           of astArrayAggregate * int

and astRecordAggregate         = SingleRecordAggregate           of astInfo * astIdentifier * astExpression
                               | MultipleRecordAggregate         of astInfo * astIdentifier * astExpression * astRecordAggregate
                               | CheckedRecordAggregate          of astRecordAggregate * astFieldTypeDenoter

and astVname                   = SimpleVname                     of astInfo * astIdentifier
                               | DotVname                        of astInfo * astVname * astIdentifier
                               | SubscriptVname                  of astInfo * astVname * astExpression
                               | CheckedVname                    of astVname * bool * bool * int * astTypeDenoter
                                                                               (* variable, indexed, offset *)

and astDeclaration             = NullDeclaration
			       | ConstDeclaration                of astInfo * astIdentifier * astExpression
                               | VarDeclaration                  of astInfo * astIdentifier * astTypeDenoter
                               | ProcDeclaration                 of astInfo * astIdentifier * astFormalParameterSequence * astCommand
                               | FuncDeclaration                 of astInfo * astIdentifier * astFormalParameterSequence * astTypeDenoter * astExpression
                               | TypeDeclaration                 of astInfo * astIdentifier * astTypeDenoter
                               | UnaryOperatorDeclaration        of astInfo * astOperator * astTypeDenoter * astTypeDenoter
                               | BinaryOperatorDeclaration       of astInfo * astOperator * astTypeDenoter * astTypeDenoter * astTypeDenoter
                               | FormalParameterDeclaration      of astInfo * astFormalParameter
                               | SequentialDeclaration           of astInfo * astDeclaration * astDeclaration

and astFormalParameter         = ConstFormalParameter            of astInfo * astIdentifier * astTypeDenoter
                               | VarFormalParameter              of astInfo * astIdentifier * astTypeDenoter
                               | ProcFormalParameter             of astInfo * astIdentifier * astFormalParameterSequence
                               | FuncFormalParameter             of astInfo * astIdentifier * astFormalParameterSequence * astTypeDenoter

and astActualParameter         = ConstActualParameter            of astInfo * astExpression
                               | VarActualParameter              of astInfo * astVname
                               | ProcActualParameter             of astInfo * astIdentifier
                               | FuncActualParameter             of astInfo * astIdentifier

and astFormalParameterSequence = EmptyFormalParameterSequence    of astInfo
                               | SingleFormalParameterSequence   of astInfo * astFormalParameter
                               | MultipleFormalParameterSequence of astInfo * astFormalParameter * astFormalParameterSequence

and astActualParameterSequence = EmptyActualParameterSequence    of astInfo
                               | SingleActualParameterSequence   of astInfo * astActualParameter
                               | MultipleActualParameterSequence of astInfo * astActualParameter * astActualParameterSequence

and astTypeDenoter             = NullTypeDenoter
                               | ErrorTypeDenoter                of astInfo
                               | AnyTypeDenoter                  of astInfo
                               | SimpleTypeDenoter               of astInfo * astIdentifier
                               | ArrayTypeDenoter                of astInfo * astIntegerLiteral * astTypeDenoter
                               | RecordTypeDenoter               of astInfo * astFieldTypeDenoter
                               | BoolTypeDenoter                 of astInfo
                               | IntTypeDenoter                  of astInfo
                               | CharTypeDenoter                 of astInfo

and astFieldTypeDenoter        = SingleFieldTypeDenoter          of astInfo * astIdentifier * astTypeDenoter
                               | MultipleFieldTypeDenoter        of astInfo * astIdentifier * astTypeDenoter * astFieldTypeDenoter

and astIntegerLiteral          = IntegerLiteral                  of astInfo * string

and astCharacterLiteral        = CharacterLiteral                of astInfo * string

and astIdentifier              = Identifier                      of astInfo * string
                               | CheckedIdentifier               of astIdentifier * astDeclaration ref

and astOperator                = Operator                        of astInfo * string
                               | CheckedOperator                 of astOperator * astDeclaration ref

