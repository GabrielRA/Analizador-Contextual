(* ----------------------------------------- *)
(* Error Reporting Library for Caml-Triangle *)
(* Implementation file                       *)
(*                                           *)
(* (c) 2006 Luis Leopoldo Pérez.             *)
(* Last modification: March 12, 2006         *)
(* ----------------------------------------- *)

open Printf
open Lexing

(* Actual error count *)
let errorCount = ref 0

(* Error kind *)
type errorKind = Error
               | Restriction

(* Error type *)
type errorType = {msg: string; pos: Lexing.position; kind: errorKind}

(* Error list type *)
type errorListType = NullErrorList
                   | ErrorList of errorType list

(* Actual error list *)
let errorList = ref NullErrorList

(* Unboxing *)
let unboxErrorList() = match !errorList with
    NullErrorList -> []
  | ErrorList (s) -> s


(* Returns the number of errors found *)
let numErrors() = !errorCount

(* Reports an error, adding it to the error list *)
let reportError e p =
    errorList:= ErrorList(unboxErrorList() @ [{msg=e;pos=p;kind=Error}]);
    incr errorCount
                      

(* Reports a restriction, adding it to the error list *)
let reportRestriction e = 
    errorList:= ErrorList(unboxErrorList() @ [{msg=e;pos=Lexing.dummy_pos;kind=Restriction}]);
    incr errorCount

(* Prints the entire error list *)
let showErrors() = 
    let rec printList l = (match l with
        (a::b) -> (match a.kind with
                         Error       -> printf "ERROR: %s [%i:%i]\n" a.msg a.pos.pos_lnum (a.pos.pos_cnum-a.pos.pos_bol)
                       | Restriction -> printf "RESTRICTION: %s\n" a.msg);
                  printList b
      | []     -> ()) in
                        printList (unboxErrorList())

(* Writes the error list in the specified XML file *)
let writeXMLErrors s =
    let rec printList c l = (match l with
        (a::b) -> (match a.kind with
                        Error       -> output_string (c) ("<error message=\"" ^ a.msg ^ "\" row=\"" ^ (string_of_int a.pos.pos_lnum) ^ "\" column=\"" ^ (string_of_int (a.pos.pos_cnum-a.pos.pos_bol)) ^ "\"/>\n")
                      | Restriction -> output_string c ("<restriction message=\"" ^ a.msg ^ "\"/>\n"));
                   printList c b
      | []     -> ()) in
    try
       let f = open_out s in
           output_string f "<?xml version=\"1.0\" standalone=\"yes\"?>\n<errorlist>\n";
           printList f (unboxErrorList());
           output_string f "</errorlist>\n";
           close_out f
    with Sys_error s -> printf "Couldn't write XML error file.\n"