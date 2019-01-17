(**
This program was originally written by Ashuin Sharma Chamorro on June 27, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
This is the scanner of compiler of language Triangle.

@author Ashuin Sharma Chamorro
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)


open Token
open ErrorReporter


(** Global Error*)
exception Lex_error of string;;


(** Global variables.*)
(** The variables have the definition of the triangle tokens *)
let digits = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']
let lc_letters =
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
   'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let uc_letters =
  ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O';
   'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']
let letters = lc_letters @ uc_letters
let op_characters =
  ['+'; '-'; '*'; '/'; '='; '<'; '>'; '\\'; '&'; '@'; '%'; '^'; '?']
let space_tab = [' '; '\t']
let blank = space_tab @ ['\n'; '\r']
let graphic = digits @ letters @ op_characters @ space_tab @
  ['.'; ':'; ';'; ','; '~'; '('; ')'; '['; ']'; '{'; '}'; '_'; '|'; '!'; '\'';
   '`'; '"'; '#'; '$']

(** The initial buffer size of 100 characters is defined, but he can increase
    arbitrarily
 *)
let current_spelling = Buffer.create 100

(** The scanner starts on line 1 *)
let line = ref 1

(** Miscellaneous Methods *)

(** Function to know if an element exists in a list *)
let rec is_in mem = function
  | [] -> false
  | x::rest -> x = mem || is_in mem rest

(** Function to convert a char into a string *)
let str_of_ch ch = (String.make 1 ch)

(** Function to know if an element exists in a digits list *)
let is_digit = fun c ->
  is_in c digits

(** Function to know if an element exists in a letter list *)
let is_letter = fun c ->
  is_in c letters

(** Function to know if an element exists in a graphic list *)
let is_graphic = fun c ->
  is_in c graphic
 
(** Function to know if an element exists in a operator list *) 
let is_operator = fun c ->
  is_in c op_characters

(** Function to know if an element exists in a blank list *)
let is_blank = fun c ->
  is_in c blank

(** Function to get a new char
    @param curr_char is the element to save the new char
    @param lb is the lex buffer given by the scanned file
 *)
let get_char lb curr_char = (
  lb.Lexing.lex_curr_pos <- lb.Lexing.lex_curr_pos + 1;
  curr_char := Lexing.lexeme_char lb (lb.Lexing.lex_curr_pos);
  lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = " ";
     Lexing.pos_lnum = (!line);
     Lexing.pos_bol = lb.Lexing.lex_curr_p.Lexing.pos_bol;
     Lexing.pos_cnum = lb.Lexing.lex_curr_pos}
)

(** Function to change the current line in order to correctly show the position
    of the errors
    @param lb is the lex buffer given by the scanned file
 *)
let change_line lb = (
  line := !line + 1;
  lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = " ";
     Lexing.pos_lnum = (!line);
     Lexing.pos_bol = lb.Lexing.lex_curr_pos;
     Lexing.pos_cnum = lb.Lexing.lex_curr_pos};
  )

(** Reads a token from the buffer and returns it
    @param lb is the lex buffer given by the scanned file
    @return Token The new token read
*)
let scan_token lb =
  let curr_char = ref (Lexing.lexeme_char lb lb.Lexing.lex_curr_pos) in
  let next_source_char () = (
    if (lb.Lexing.lex_curr_pos < (lb.Lexing.lex_buffer_len-1)) then begin
        get_char lb curr_char
    end else begin
       curr_char := '\000'
    end
  ) in

  (** Function that reads a new char and replaces the current char *)
  let take_it () = (
    Buffer.add_char current_spelling !curr_char;
    next_source_char()
  ) in

  (** Function that reads a new character and only accepts a specific character
      after which it replaces the current character.
  *)
  let take exp_ch = (
    if (!curr_char = exp_ch) then
      take_it()
    else begin
      let error_message = "Lexical Error. Expected " ^ str_of_ch(exp_ch)
        ^ " found " ^ str_of_ch(!curr_char) in
      ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
      raise (Lex_error "invalid character")
    end
  ) in

  (** Function that cleans a blank character or a comment *)
  let scan_separator () = (
    if (!curr_char = '!') then begin
      take_it();
      while (is_graphic !curr_char) do
        take_it()
      done;
      if (!curr_char = '\n') then begin
          take_it();
          if (!curr_char = '\r') then begin
            take_it()
          end;
          change_line lb
        end else
      if (!curr_char = '\r') then begin
          take_it();
          if (!curr_char = '\n') then begin
            take_it()
          end;
          change_line lb
        end
      else begin
        let error_message = "Lexical Error. Expected change of line found "
          ^ str_of_ch(!curr_char) in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Lex_error "invalid character")
      end
    end else begin
      if (!curr_char = '\n') then begin
          take_it();
          if(!curr_char = '\r') then begin
           take_it()
          end;
          change_line lb
        end else
      if (!curr_char = '\r') then begin
          take_it();
          if(!curr_char = '\n') then begin
           take_it()
          end;
         change_line lb
        end
      else
        take_it()
    end
  )

  (** Function that reads a set of characters and checks that they set a token
      otherwise triggers an error
  *)
  and scan () = (
    if (is_letter !curr_char) then begin
        take_it();
        while ((is_letter !curr_char) || (is_digit !curr_char)) do
          take_it()
        done;
        match (Buffer.contents current_spelling) with
        | "array" -> Token.Array
        | "begin" -> Token.Begin
        | "const" -> Token.Const
        | "do" -> Token.Do
        | "else" -> Token.Else
        | "end" -> Token.End
        | "func" -> Token.Func
        | "if" -> Token.If
        | "in" -> Token.In
        | "let" -> Token.Let
        | "of" -> Token.Of
        | "proc" -> Token.Proc
        | "record" -> Token.Record
        | "then" -> Token.Then
        | "type" -> Token.Type
        | "var" -> Token.Var
        | "while" -> Token.While
        | _ -> Token.Identifier (Buffer.contents current_spelling)
      end else
    if (is_digit !curr_char) then begin
        take_it();
        while (is_digit !curr_char) do
          take_it()
        done;
        Token.Int_literal (Buffer.contents current_spelling)
      end else
    if (!curr_char= '\'') then begin
        take_it();
        if (is_graphic !curr_char ) then begin
          take_it();
          take '\'';
          Token.Char_literal (Buffer.contents current_spelling)
        end else begin
          let error_message =
            "Lexical Error. Expected a graphic character found "
            ^ str_of_ch(!curr_char) in
          ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
          raise (Lex_error "invalid character")
        end
      end else
    if (is_operator !curr_char) then begin
        take_it();
        while(is_operator !curr_char) do
          take_it()
        done;
        Token.Operator (Buffer.contents current_spelling)
      end
    else begin
      match (!curr_char) with
      | '.' -> begin take_it(); Token.Dot end
      | ';' -> begin take_it(); Token.Semicolon end
      | ',' -> begin take_it(); Token.Comma end
      | '~' -> begin take_it(); Token.Is end
      | '(' -> begin take_it(); Token.Lparen end
      | ')' -> begin take_it(); Token.Rparen end
      | '[' -> begin take_it(); Token.Lbracket end
      | ']' -> begin take_it(); Token.Rbracket end
      | '{' -> begin take_it(); Token.Lcurly end
      | '}' -> begin take_it(); Token.Rcurly end
      | '\000' -> begin take_it(); Token.Eof end
      | ':' ->
        begin
          take_it();
          match (!curr_char) with
          | '=' ->
            begin
              take_it();
              Token.Becomes
            end
          | _ -> Token.Colon
        end
      | _ ->
        begin
          let error_message = "Invalid Character found. " in
          ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
          raise (Lex_error "invalid character")
        end
    end;
  ) in

  (** All existing garbage is cleaned before a token *)
  while (!curr_char = '!' ||  (is_blank !curr_char)) do
    scan_separator()
  done;

  Buffer.clear current_spelling;
  scan();;