(* --------------------------------------------- *)
(* Runtime Entities Definition for Caml-Triangle *)
(* Interface file                                *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: May 26, 2006               *)
(* --------------------------------------------- *)


type objectAddress = { level: int; displacement: int }

                                           
type runtimeEntity = NullRuntimeEntity     
                   | KnownValue         of int*int (* size,value *)
                   | UnknownValue       of int*objectAddress (* size *)
                   | KnownAddress       of int*objectAddress (* size *)
                   | UnknownAddress     of int*objectAddress (* size *)
                   | KnownRoutine       of int*objectAddress (* size *)
                   | UnknownRoutine     of int*objectAddress (* size *)
                   | PrimitiveRoutine   of int*int (* size, displacement *)
                   | EqualityRoutine    of int*int (* size, displacement *)
                   | Field              of int*int (* size, fieldOffset *)
                   | TypeRepresentation of int     (* size *)