(* ----------------------------------------- *)
(* Error Reporting Library for Caml-Triangle *)
(* Interface file                            *)
(*                                           *)
(* (c) 2006 Luis Leopoldo Pérez.             *)
(* Last modification: March 12, 2006         *)
(* ----------------------------------------- *)

(* Returns the number of errors found *)
val numErrors: unit -> int

(* Reports an error, adding it to the error list *)
val reportError: string -> Lexing.position -> unit

(* Reports a restriction, *)
val reportRestriction: string -> unit

(* Prints the entire error list *)
val showErrors: unit -> unit

(* Writes the error list in the specified XML file *)
val writeXMLErrors: string -> unit