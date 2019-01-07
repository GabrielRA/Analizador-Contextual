(* ------------------------------------ *)
(* Program entrypoint for Caml-Triangle *)
(* Implementation file                  *)
(*                                      *)
(* (c) 2006 Luis Leopoldo Pérez.        *)
(* Last modification: March 12, 2006    *)
(* ------------------------------------ *)

open Ast
open Parser
open Scanner
open Checker
open Encoder
open ErrorReporter
open Printf
open Lexing

let outputFile = ref "a.out"
let xmlErrorFile = ref ""
let xmlTreeFile = ref ""
let xmlDTreeFile = ref ""
let xmlTableFile = ref ""
let doUsage = ref false
let astree = ref Null_program
let dastree = ref Null_program

(* Compiles the program! *)

let compile inpt outpt =
    let readfile name = (
        let chan = open_in name in
        let len = (in_channel_length chan) in
        let buf = String.make len ' ' in
        let m = input chan buf 0 len in
            close_in_noerr chan;
            buf)
    and parse buf = (
        let lexbuf = (Lexing.from_string buf) in
            try        
               astree := Parser.parseProgram Scanner.scanToken lexbuf
            with _ -> ()) in
    let zer = readfile inpt in
        printf "Syntactic Analysis ...\n";
        parse zer;
        if (ErrorReporter.numErrors() == 0 && !astree != Null_program) then
        begin
           printf "Contextual Analysis ...\n";
           dastree := Checker.check_program !astree;
           if (ErrorReporter.numErrors() == 0) then
           begin
              printf "Code Generation ...\n";
              Encoder.encodeProgram !dastree outpt
           end           
        end;
        ErrorReporter.showErrors();
        if (ErrorReporter.numErrors() == 0) then
           printf "Compilation was successful\n"
        else
        begin
           astree := Null_program;
           dastree := Null_program;
           printf "Compilation was unsuccessful\n"
        end

let specs = [
    ("-o", Arg.String (function s -> outputFile:= s), "<file> Sets the output file name. Defaults to a.out");
    ("-xe", Arg.String (function s -> xmlErrorFile:= s), "<file> Writes the error list in XML format.");
    ("-xt", Arg.String (function s -> xmlTreeFile:= s), "<file> Writes the abstract syntax tree in XML format.");
    ("-xd", Arg.String (function s -> xmlDTreeFile:= s), "<file> Writes the decorated abstract syntax tree in XML format.");
    ("-xi", Arg.String (function s -> xmlTableFile:= s), "<file> Writes the identification table details in XML format.")]

let usage_msg = "\nusage: " ^ Sys.argv.(0) ^ " <source> [options]\n"

let main () = 
    printf "********** Triangle Compiler (Caml Version 1.0) **********\n";
    if ((Array.length Sys.argv) < 2) then
    begin
       printf "%s: not enough parameters." Sys.argv.(0);
       Arg.usage specs usage_msg
    end
    else
    let inputFile = Sys.argv.(1) in
        incr(Arg.current);
        Arg.parse specs (function s -> ()) (usage_msg);
        try
           compile inputFile !outputFile;
           if ((String.compare !xmlErrorFile "") != 0) then
              ErrorReporter.writeXMLErrors !xmlErrorFile;

           if ((String.compare !xmlTreeFile "") != 0) then
              TreeDrawer.writeXMLTree !astree !xmlTreeFile;

           if ((String.compare !xmlDTreeFile "") != 0) then
              TreeDrawer.writeXMLTree !dastree !xmlDTreeFile;
              
           
           if ((String.compare !xmlTableFile "") != 0) then
              Encoder.writeXMLTable !xmlTableFile
           
              
        with Sys_error s -> printf "%s" s


let _ = Printexc.print main ()