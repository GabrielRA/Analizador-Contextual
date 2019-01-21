(**
This program was originally written by Luis Leopoldo Pérez on March 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019..
Error Reporting Library for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
@author Christian León Guevara
@author Gabriel Ramírez Ramírez
*)

open Printf
open Lexing

(** Actual error count *)
let error_count = ref 0

(** Error kind *)
type error_kind = Error | Restriction

(** Error type *)
type error_type = {msg: string; pos: Lexing.position; kind: error_kind}

(** Error list type *)
type error_list_type = Null_error_list | Error_list of error_type list

(** Actual error list *)
let error_list = ref Null_error_list

(** Unboxing *)
let unbox_error_list () =
  match !error_list with
    Null_error_list -> []
  | Error_list (s) -> s


(** Returns the number of errors found *)
let num_errors() = !error_count

(** Reports an error, adding it to the error list *)
let report_error e p =
  error_list :=
    Error_list(unbox_error_list() @ [{msg = e; pos = p; kind = Error}]);
  incr error_count
                      

(** Reports a restriction, adding it to the error list *)
let report_restriction e = 
  error_list :=
    Error_list(unbox_error_list()
    @ [{msg = e; pos = Lexing.dummy_pos; kind = Restriction}]);
  incr error_count

(** Prints the entire error list *)
let show_errors() = 
  let rec print_list l = (
    match l with
    (a::b) -> (
      match a.kind with
        Error ->
          let column = (a.pos.pos_cnum - a.pos.pos_bol) in
          printf "ERROR: %s [%i:%i]\n" a.msg a.pos.pos_lnum column
      | Restriction -> 
        printf "RESTRICTION: %s\n" a.msg);
        print_list b
    | [] -> ()
  ) in
  print_list (unbox_error_list())

(** Writes the error list in the specified XML file *)
let write_x_m_l_errors s =
  let rec print_list c l = (
    match l with
      (a::b) ->
        (
          match a.kind with
            Error ->
            let error_message = 
              "<error message=\"" ^ a.msg ^ "\" row=\""
              ^ (string_of_int a.pos.pos_lnum) ^ "\" column=\""
              ^ (string_of_int (a.pos.pos_cnum-a.pos.pos_bol)) ^ "\"/>\n" in
            output_string (c) error_message
          | Restriction ->
            output_string c ("<restriction message=\"" ^ a.msg ^ "\"/>\n"));
          print_list c b
    | []     -> ()) in
    try
      let f = open_out s in
        let header =
          "<?xml version=\"1.0\" standalone=\"yes\"?>\n<errorlist>\n" in
        output_string f header;
        print_list f (unbox_error_list());
        output_string f "</errorlist>\n";
        close_out f
    with Sys_error s -> printf "Couldn't write XML error file.\n"