(* --------------------------------------------- *)
(* Identification Table Module for Caml-Triangle *)
(* Interface file                                *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: April 21, 2006             *)
(* --------------------------------------------- *)

open Ast

type idEntry = {mutable id: string; mutable attr: astDeclaration ref; mutable levl: int}

val open_scope: unit -> unit
val close_scope: unit -> unit
val enter: string -> astDeclaration ref -> unit
val exists: string -> bool
val retrieve: string -> astDeclaration ref
val retrieve_element: string -> idEntry