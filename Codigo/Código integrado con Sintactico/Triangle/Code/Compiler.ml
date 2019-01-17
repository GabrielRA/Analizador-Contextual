(**
This program was originally written by Luis Leopoldo Pérez on March 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by 
students of ITCR in January 2019.
Program entrypoint for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

open Ast
open Parser
open Scanner
open ErrorReporter
open Printf
open Lexing

(** File output variables for printers *)
let output_file = ref "a.out"
let xml_error_file = ref ""
let xml_tree_file = ref ""
let xml_decorated_tree_file = ref ""
let tokensFile_Pipe = ref ""
let tokensFile_XML = ref ""
let tokensFile_HTML = ref ""
let do_usage = ref false

(** Variables for tree storage *)
let astree = ref Null_program
let dastree = ref Null_program

(* Compiles the program! *)


(** Function that the name of a file reads *)
let readfile file = (
  let ic = open_in file in
  let buf = Buffer.create (in_channel_length ic) in
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
    done;
    assert false
  with 
    End_of_file -> Buffer.contents buf 
)


(** 
Function generates impression of tokens separated by pipes
@param inpt Source file
@param file_name Destination file 
*)
let printTokensFuntion_Pipe inpt file_name =
  printf "Generate tokens with pipes....\n";    
  let lexbuf = (Lexing.from_string (readfile inpt)) in
  try
    TokenPrinter_Pipe.print_tokens Scanner.scan_token lexbuf file_name;
  with 
    _ -> ()

(** 
Function generates impression of tokens in xml file
@param inpt Source file
@param file_name Destination file 
*)
let printTokensFuntion_XML inpt file_name =
  printf "Generate tokens in xml file....\n";
  let lexbuf = (Lexing.from_string (readfile inpt)) in
  try
    TokenPrinter_XML.print_tokens Scanner.scan_token lexbuf file_name;
  with
    _ -> ()

(** 
Function generates impression of tokens in HTML file
@param inpt Source file
@param file_name Destination file 
*)
let printTokensFuntion_HTML inpt file_name =
  printf "Generate tokens in html file....\n";  
  let lexbuf = (Lexing.from_string (readfile inpt)) in
  try
    TokenPrinter_HTML.print_tokens Scanner.scan_token lexbuf file_name;
  with
    _ -> ()      

(** 
Function that compiles a source file
@param inpt Source file
@param outpt Destination file 
*)
let compile inpt outpt =
  let parse buf = (
  let lexbuf = (Lexing.from_string buf) in
    try
      astree := Parser.parse_program Scanner.scan_token lexbuf
    with
      _ -> ()
  ) in
  let zer = readfile inpt in
  printf "Syntactic Analysis ...\n";
  parse zer;
  (*  Uncomment the following lines to perform the contextual analysis and
      the generation of code *)
  
  if (ErrorReporter.num_errors() == 0 && !astree != Null_program) then
  begin
     printf "Contextual Analysis ...\n";
     dastree := Checker.check_program !astree;
     (* Uncomment the generation of code *)
     (*
     if (ErrorReporter.num_errors() == 0) then
     begin
        printf "Code Generation ...\n";
        Encoder.encode_program !dastree outpt
     end
     *)           
  end;
  
      
  ErrorReporter.show_errors();
  if (ErrorReporter.num_errors() == 0) then
    printf "Compilation was successful\n"
  else begin
    astree := Null_program;
    dastree := Null_program;
    printf "Compilation was unsuccessful\n"
  end

(** Arguments to send by command lines and their different meanings  *)
let specs = [
  ("-o", Arg.String (function s -> output_file:= s),
    "<file> Sets the output file name. Defaults to a.out");
  ("-xe", Arg.String (function s -> xml_error_file:= s),
    "<file> Writes the error list in XML format.");
  ("-xt", Arg.String (function s -> xml_tree_file:= s),
    "<file> Writes the abstract syntax tree in XML format.");
  ("-tpp", Arg.String (function s -> tokensFile_Pipe:= s),
    "<file> Writes the tokens in file separated with pipes.");
  ("-tpx", Arg.String (function s -> tokensFile_XML:= s),
    "<file> Writes the tokens in XML file.");
  ("-tph", Arg.String (function s -> tokensFile_HTML:= s),
    "<file> Writes the tokens in HTML file.");
  ("-xd", Arg.String (function s -> xml_decorated_tree_file:= s),
    "<file> Writes the decorated abstract syntax tree in XML format.")]

(** General message *)
let usage_msg = "\nusage: " ^ Sys.argv.(0) ^ " <source> [options]\n"

(** Main function *)
let main () = 
  printf "********** Triangle Compiler (Caml Version 1.0) **********\n";
  if ((Array.length Sys.argv) < 2) then begin
     printf "%s: not enough parameters." Sys.argv.(0);
     Arg.usage specs usage_msg
  end else
  let input_file = Sys.argv.(1) in
  incr(Arg.current);
  Arg.parse specs (function s -> ()) (usage_msg);
  try
    compile input_file !output_file;
    if ((String.compare !xml_error_file "") != 0) then
      ErrorReporter.write_x_m_l_errors !xml_error_file;
    if ((String.compare !xml_tree_file "") != 0) then
      TreeDrawer.write_x_m_l_tree !astree !xml_tree_file;
    if ((String.compare !tokensFile_Pipe "") != 0) then
      printTokensFuntion_Pipe input_file !tokensFile_Pipe;
    if ((String.compare !tokensFile_XML "") != 0) then
      printTokensFuntion_XML input_file !tokensFile_XML;
    if ((String.compare !tokensFile_HTML "") != 0) then
      printTokensFuntion_HTML input_file !tokensFile_HTML;
    if ((String.compare !xml_decorated_tree_file "") != 0) then
      TreeDrawer.write_x_m_l_tree !dastree !xml_decorated_tree_file
  with 
    Sys_error s -> printf "%s" s

let _ = Printexc.print main ()