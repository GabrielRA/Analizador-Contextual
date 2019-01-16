(* ----------------------------------------- *)
(* Error Reporting Library for Caml-Triangle *)
(* Interface file                            *)
(*                                           *)
(* (c) 2006 Luis Leopoldo Pérez.             *)
(* Last modification: March 12, 2006         *)
(* ----------------------------------------- *)

(* Returns the number of errors found *)
val num_errors: unit -> int

(* Reports an error, adding it to the error list *)
val report_error: string -> Lexing.position -> unit

(* Reports a restriction, *)
val report_restriction: string -> unit

(* Prints the entire error list *)
val show_errors: unit -> unit

(* Writes the error list in the specified XML file *)
val write_XML_errors: string -> unit