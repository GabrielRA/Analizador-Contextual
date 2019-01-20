(* --------------------------------------------- *)
(* Identification Table Module for Caml-Triangle *)
(* Interface file                                *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: April 21, 2006             *)
(* --------------------------------------------- *)

open Ast
open Id_entry

val open_scope: unit -> unit
val close_scope: unit -> unit
val enter: string -> ast_declaration ref -> unit
val exists: string -> bool
val retrieve: string -> ast_declaration ref
val retrieve_element: string -> id_entry