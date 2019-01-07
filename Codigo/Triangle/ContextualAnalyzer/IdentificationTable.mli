(* --------------------------------------------- *)
(* Identification Table Module for Caml-Triangle *)
(* Interface file                                *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: April 21, 2006             *)
(* --------------------------------------------- *)

open Ast

type idEntry = {mutable id: string; mutable attr: astDeclaration ref; mutable levl: int}

val openScope: unit -> unit
val closeScope: unit -> unit
val enter: string -> astDeclaration ref -> unit
val exists: string -> bool
val retrieve: string -> astDeclaration ref
val retrieveElement: string -> idEntry