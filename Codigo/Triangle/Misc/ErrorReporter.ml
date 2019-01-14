(*
This program was made by Luis Leopoldo Pérez at March 12, 2006.
This program was repaired, completed, verified and validated by students
of ITCR at 2018.
Error Reporting Library for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Christian León Guevara
@author Gabriel Ramírez Ramírez

Last modification: January, 2019
*)

open Printf
open Lexing

(* Actual error count *)
let error_count = ref 0

(* Error kind *)
type error_kind = 
  Error 
| Restriction

(* Error type *)
type error_type = {msg: string; pos: Lexing.position; kind: error_kind}

(* Error list type *)
type error_list_type = 
  Null_error_list 
| Error_list of error_type list

(* Actual error list *)
let error_list = ref Null_error_list

(* Unboxing *)
let unbox_error_list() = 
  match !Error_list with
    Null_error_list -> []
  | Error_list (s) -> s


(* Returns the number of errors found *)
let num_errors() = !error_count

(* Reports an error, adding it to the error list *)
let report_error e p =
  error_list:= 
    Error_list(unbox_error_list() @ [{msg = e; pos = p; kind = Error}]);
    incr error_count                      

(* Reports a restriction, adding it to the error list *)
let report_restriction e = 
  error_list:= 
    Error_list(
	  unbox_error_list() 
	  @ [{msg = e; pos = Lexing.dummy_pos; kind = Restriction}]
	);
    incr error_count

(* Prints the entire error list *)
let show_errors() = 
  let rec print_list l = (
	match l with
      (a::b) -> (
	    match a.kind with
	      Error -> 
		    printf "ERROR: %s [%i:%i]\n" a.msg a.pos.pos_lnum (a.pos.pos_cnum-a.pos.pos_bol)
        | Restriction -> printf "RESTRICTION: %s\n" a.msg);
	      print_list b
      | [] -> ()
    ) in
    print_list (unbox_error_list())

	
(* Writes the error list in the specified XML file *)
let write_XML_errors s =
  let rec print_list c l = (
	match l with
      (a::b) -> (
	    match a.kind with
          Error -> 
		    output_string (c) ("<error message=\"" 
			^ a.msg ^ "\" row=\"" ^ (string_of_int a.pos.pos_lnum)
			^ "\" column=\"" 
			^ (string_of_int (a.pos.pos_cnum-a.pos.pos_bol)) ^ "\"/>\n")
        | Restriction -> 
		    output_string c ("<restriction message=\"" ^ a.msg ^ "\"/>\n"));
            print_list c b
    | [] -> ()) in
    try
	  let f = open_out s in
        output_string f "<?xml version=\"1.0\" standalone=\"yes\"?>\n<Error_list>\n";
        print_list f (unbox_error_list());
        output_string f "</Error_list>\n";
        close_out f
    with Sys_error s -> printf "Couldn't write XML error file.\n"