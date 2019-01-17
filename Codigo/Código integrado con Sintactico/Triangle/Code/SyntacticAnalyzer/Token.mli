(**
This program was written by students of ITCR in January 2019.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Definition of tokens for Caml-Triangle
Interface file                                    

@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

(** 
 This type contains all the different types of possible tokens
*)
type token =
  | Int_literal of (string)
  | Char_literal of (string)
  | Identifier of (string)
  | Operator of (string)
  | Array
  | Begin
  | Const
  | Do
  | Else
  | End
  | Func
  | If
  | In
  | Let
  | Of
  | Proc
  | Record
  | Then
  | Type
  | Var
  | While
  | Dot
  | Colon
  | Semicolon
  | Comma
  | Becomes
  | Is
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lcurly              
  | Rcurly
  | Eof