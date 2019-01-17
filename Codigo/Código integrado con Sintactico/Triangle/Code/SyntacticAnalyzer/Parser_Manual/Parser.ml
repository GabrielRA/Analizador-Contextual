(**
This program was written by students of ITCR in January 2019.
This program is the Parser component of the Triangle's language compiler.

@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde

*)


open Ast
open ErrorReporter
open Lexing
open RuntimeEntity
open Token

(** This is the declaration of exception of program *)
exception Synt_error of string;;

(**
This function is used to report errors.

@param in_token Is the input token
@return returns a string that decribes the token.
*)
let token_to_str in_token = (
  match in_token with
    Int_literal(a) -> "integer literal"
  | Char_literal(a) -> "character literal"
  | Identifier(a) -> "identifier"
  | Operator(a) -> "operator"
  | Array -> "array"
  | Begin -> "begin"
  | Const -> "const"
  | Do -> "do"
  | Else -> "else"
  | End -> "end"
  | Func -> "func"
  | If -> "if"
  | In -> "in"
  | Let -> "let"
  | Of -> "of"
  | Proc -> "proc"
  | Record -> "record"
  | Then -> "then"
  | Type -> "type"
  | Var -> "var"
  | While -> "while"
  | Dot -> "."
  | Colon -> ":"
  | Semicolon -> ";"
  | Comma -> ","
  | Becomes -> ":="
  | Is -> "~"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lcurly -> "{"
  | Rcurly -> "}"
  | Eof -> "end of file");;

(**
  This is the main function.

  @param scan_func is the function given by scanner
  @param lb is the lex buffer given by scanner

  @return The Ast program in there aren't errors, in the other case
  returns Null_program
*)
let parse_program scan_func lb = (

  (** current_token is the principal variable of function, the ref (Eof) is
  only the initial value. *)
  let rec current_token = ref (Eof)


  (** This funcion is to move along the chain of tokens
    @return the next token usign the functions of scanner.
  *)
  and accept_it () = current_token := scan_func lb

  (**
    This function is to get an especific token from the chain of tokens.
    @param expected_token is the input token
    @return the next token if is correct, otherwise raise an exception.
  *)
  and accept expected_token = (
    if (!current_token == expected_token) then
      accept_it()
    else begin
      let error_message = token_to_str (!current_token) ^ " expected here" in
      ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
      raise (Synt_error "Syntactical error")
    end
  )

  (*   parse_integer_literal parses an integer-literal, and constructs
       a leaf Ast to represent it. *)
  and parse_integer_literal () = (
    match !current_token with
    | Int_literal(a) ->
      begin
        let ast =
          Integer_literal({pos = lb.lex_curr_p; run = Null_runtime_entity}, a)
        in
        accept_it();
        ast
      end
    | _ ->
      begin
        let error_message = token_to_str(!current_token) ^ "  expected here" in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Synt_error "Syntactical error")
      end
  )

  (*   parse_char_literal parses an char-literal, and constructs
       a leaf Ast to represent it. *)
  and parse_char_literal () = (
    match !current_token with
    | Char_literal(a) ->
      begin
        let ast =
          Character_literal({pos = lb.lex_curr_p; run = Null_runtime_entity}, a)
        in
        accept_it();
        ast
        end
    | _ ->
      begin
        let error_message = token_to_str(!current_token) ^ "  expected here" in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Synt_error "Syntactical error")
      end
  )

  (*   parse_identifier parses an identifier-literal, and constructs
       a leaf Ast to represent it. *) 
  and parse_identifier () = (
    match !current_token with
    | Identifier(a) ->
      begin
        let ast =
          Ast.Identifier({pos = lb.lex_curr_p; run = Null_runtime_entity}, a) in
        accept_it();
        ast
      end
    | _ ->
      begin
        let error_message = token_to_str(!current_token) ^ "  expected here" in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Synt_error "Syntactical error")
      end
  )

  (*   parse_operator parses an operator-literal, and constructs
       a leaf Ast to represent it. *) 
  and parse_operator () = (
    match !current_token with
    | Operator(a) ->
      begin
        let ast =
          Ast.Operator({pos = lb.lex_curr_p; run = Null_runtime_entity}, a) in
        accept_it();
        ast
      end
    | _ ->
      begin
        let error_message = token_to_str(!current_token) ^ "  expected here" in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Synt_error "Syntactical error")
      end
  )

  (*  parse_single_command is part of parse_command (read below). *)
  and parse_single_command ()  = (
    match !current_token with
    | Identifier(a) ->
      begin
        let i_ast = (parse_identifier()) in
        match !current_token with
        | Lparen ->
          begin
            accept_it();
            let aps_ast = (parse_actual_parameter_sequence()) in
            accept Rparen;
            let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in 
            Call_command(ast_info, i_ast, aps_ast)
          end
        | _ ->
          begin
            let v_ast = parse_rest_of_vname(i_ast) in
            accept Becomes;
            let e_ast = (parse_expression()) in
            let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in 
            Assign_command(ast_info, v_ast, e_ast)
          end
      end
    | Begin ->
      begin
        accept_it();
        let c_ast = parse_command() in
        accept End;
        c_ast
      end
    | Let ->
      begin
        accept_it();
        let d_ast = parse_declaration() in
        accept In;
        let sc_ast = parse_single_command() in
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
        Let_command(ast_info, d_ast, sc_ast)
      end
    | If ->
      begin
        accept_it();
        let e_ast = parse_expression() in
        accept Then;
        let sc1_ast = parse_single_command() in
        accept Else;
        let sc2_ast = parse_single_command() in
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in 
        If_command(ast_info, e_ast, sc1_ast, sc2_ast)
      end
    | While ->
      begin
        accept_it();
        let e_ast = parse_expression() in
        accept Do;
        let sc_ast = parse_single_command() in
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
        While_command(ast_info, e_ast, sc_ast)
       end
    | Semicolon
    | End
    | Else
    | In
    | Eof ->
      Empty_command({pos = lb.lex_curr_p; run = Null_runtime_entity})
    | _ ->
      begin
        let error_message = 
          token_to_str(!current_token) ^ "  cannot start a command" in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Synt_error "Syntactical error")
      end
  )

  (*parse_command parses the command, and constructs an Ast
    to represent its phrase structure. *)
  and parse_command ()  = (
    let c1_ast = ref (parse_single_command()) in
    while (!current_token == Semicolon) do
      accept_it();
      let c2_ast = parse_single_command() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      c1_ast := Sequential_command(ast_info, !c1_ast, c2_ast)   
    done;
    !c1_ast
  )


  (*parse_expression parses the expression and constructs an Ast to represent
    its structure.
    This function uses other functions to divide the problem and check
    correctly the expression.
  *)
  and parse_expression ()  = (
    match !current_token with
    | Let ->
      accept_it();
      let d_ast = parse_declaration() in
      accept In;
      let e_ast = parse_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = Let_expression(ast_info, d_ast, e_ast) in
      expression_ast
    | If ->
      accept_it();
      let e1_ast = parse_expression() in
      accept Then;
      let e2_ast = parse_expression() in
      accept Else;
      let e3_ast = parse_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = If_expression(ast_info, e1_ast, e2_ast, e3_ast) in
      expression_ast
    | _ ->
      let expression_ast = parse_secondary_expression() in
      expression_ast
  )

  (*parse_record_aggregate parses the expression of Record and returns an Ast.*)
  and parse_record_aggregate ()  = (
    let i_ast = parse_identifier() in
    accept Is;
    let e_ast = parse_expression() in
    if (!current_token == Comma) then begin
      accept_it();
      let a_ast = parse_record_aggregate() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let aggregate_ast =
        Multiple_record_aggregate(ast_info, i_ast, e_ast, a_ast) in
      aggregate_ast
    end else begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let aggregate_ast = Single_record_aggregate(ast_info, i_ast, e_ast) in
      aggregate_ast
    end
  )

  (*parse_array_aggregate parses the expression of Array and returns an Ast. *)
  and parse_array_aggregate ()  = (
    let e_ast = parse_expression() in
    if (!current_token == Comma) then begin
      accept_it();
      let a_ast = parse_array_aggregate() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let aggregate_ast = Multiple_array_aggregate(ast_info, e_ast, a_ast) in
      aggregate_ast
    end else begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let aggregate_ast = Single_array_aggregate(ast_info, e_ast) in
      aggregate_ast
    end
  )


  (** parse_primary_expression parses an estricted form of expression *)
  and parse_primary_expression ()  = (
    match !current_token with
    | Int_literal(a) ->
      let i1_ast = parse_integer_literal() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = Integer_expression(ast_info, i1_ast) in
      expression_ast
    | Char_literal(a) ->
      let c1_ast = parse_char_literal() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = Character_expression(ast_info, c1_ast) in
      expression_ast
    | Lbracket ->
      accept_it();
      let aa_ast = parse_array_aggregate() in
      accept Rbracket;
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = Array_expression(ast_info, aa_ast) in
      expression_ast
    | Lcurly ->
      accept_it();
      let ra_ast = parse_record_aggregate() in
      accept Rcurly;
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = Record_expression(ast_info, ra_ast) in
      expression_ast
    | Identifier(a) ->
      let i_ast = parse_identifier() in
      if (!current_token == Lparen) then begin
        accept_it();
        let aps_ast = parse_actual_parameter_sequence() in
        accept Rparen;
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
        let expression_ast = Call_expression(ast_info, i_ast, aps_ast) in
        expression_ast
      end else begin
        let v_ast = (parse_rest_of_vname(i_ast)) in
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
        let expression_ast = Vname_expression(ast_info, v_ast) in
        expression_ast      
      end
    | Operator(a) ->
      let op_ast = parse_operator() in
      let e_ast = parse_primary_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let expression_ast = Unary_expression(ast_info, op_ast, e_ast) in
      expression_ast
    | Lparen ->
      accept_it();
      let expression_ast = parse_expression() in
      accept Rparen;
      expression_ast
    | _ ->
      begin
        let error_message =
          token_to_str(!current_token) ^ "  cannot start an expression" in
        ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
        raise (Synt_error "Syntactical error")
      end
  )

  (** parse_secondary_expression parses an estricted form of expression *)
  and parse_secondary_expression ()  = (
    let expression_ast = ref (parse_primary_expression()) in
    while (
      match !current_token with
      | Operator(a) -> true
      | _ -> false) 
    do
      let op_ast = parse_operator() in
      let e2_ast = parse_primary_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      expression_ast :=
        Binary_expression(ast_info, !expression_ast, op_ast, e2_ast)
    done;
    !expression_ast
  )

  (*parse_vname parses the name of variable and checks if the variable
    is from record and return the Ast *)
  and parse_vname ()  =
    let i_ast = parse_identifier() in
    let v_ast = parse_rest_of_vname(i_ast) in
    v_ast

  (** parse_rest_of_vname parses identifier with '.' or '[' and return the Ast*)
  and parse_rest_of_vname i_ast  = (
    let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
    let v_ast = ref (Simple_vname(ast_info, i_ast)) in
    while (!current_token == Dot || !current_token == Lbracket) do
      if (!current_token == Dot) then begin
        accept_it();
        let i_ast = parse_identifier() in
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
        v_ast := Dot_vname(ast_info, !v_ast, i_ast)
      end else begin
        accept_it();
        let e_ast = parse_expression() in
        accept Rbracket;
        let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
        v_ast := Subscript_vname(ast_info, !v_ast, e_ast)
      end
    done;
    !v_ast
  )

  (** parse_single_declaration parses a restricted form of declaration and
      return the Ast *)
  and parse_single_declaration ()  = (
    match !current_token with
    | Const ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Is;
      let e_ast = parse_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let declaration_ast = Const_declaration(ast_info, i_ast, e_ast) in
      declaration_ast
    | Var ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Colon;
      let t_ast = parse_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let declaration_ast = Var_declaration(ast_info, i_ast, t_ast) in
      declaration_ast
    | Proc ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Lparen;
      let fps_ast = parse_formal_parameter_sequence() in
      accept Rparen;
      accept Is;
      let c_ast = parse_single_command() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let declaration_ast = Proc_declaration(ast_info, i_ast, fps_ast, c_ast) in
      declaration_ast
    | Func ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Lparen;
      let fps_ast = parse_formal_parameter_sequence() in
      accept Rparen;
      accept Colon;
      let t_ast = parse_type_denoter() in
      accept Is;
      let e_ast = parse_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let declaration_ast =
        Func_declaration(ast_info, i_ast, fps_ast, t_ast, e_ast) in
      declaration_ast
    | Type ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Is;
      let t_ast = parse_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let declaration_ast = Type_declaration(ast_info, i_ast, t_ast) in
      declaration_ast
    | _ ->
      let error_message =
        token_to_str(!current_token) ^ "  cannot start a declaration" in
      ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
      raise (Synt_error "Syntactical error")
  )

  (** parse_declaration parses the declarations that are used to produce
      bindings, and return the Ast *)
  and parse_declaration ()  = (
    let declaration_ast = ref (parse_single_declaration()) in
    while !current_token == Semicolon do
      accept_it();
      let d2_ast = parse_single_declaration() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      declaration_ast :=
        Sequential_declaration(ast_info, !declaration_ast, d2_ast)
    done;
    !declaration_ast
    )

  (** parse_proper_formal_parameter_sequence parses the sequence of parameteres
      with parse_formal_parameter *)
  and parse_proper_formal_parameter_sequence ()  = (
    let fp_ast = parse_formal_parameter() in
    if (!current_token == Comma) then begin
      accept_it();
      let fps_ast = parse_proper_formal_parameter_sequence() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formals_ast =
        Multiple_formal_parameter_sequence(ast_info, fp_ast, fps_ast) in
      formals_ast
    end else begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formals_ast = Single_formal_parameter_sequence(ast_info, fp_ast) in
      formals_ast
    end
  )

  (** parse_formal_parameter_sequence return the Ast of Formal parameter
      sequence.*)
  and parse_formal_parameter_sequence ()  = (
    if (!current_token == Rparen) then begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formals_ast = Empty_formal_parameter_sequence(ast_info) in
      formals_ast
    end else begin
      let formals_ast = parse_proper_formal_parameter_sequence() in
      formals_ast
    end
  )

  (** parse_formal_parameter parses the parameteres and checks the type
      denoter *)
  and parse_formal_parameter ()  = (
    match !current_token with
    | Identifier(a) ->
      let i_ast = parse_identifier() in
      accept Colon;
      let t_ast = parse_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formal_ast = Const_formal_parameter(ast_info, i_ast, t_ast) in
      formal_ast
    | Var ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Colon;
      let t_ast = parse_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formal_ast = Var_formal_parameter(ast_info, i_ast, t_ast) in
      formal_ast
    | Proc ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Lparen;
      let fps_ast = parse_formal_parameter_sequence() in
      accept Rparen;
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formal_ast = Proc_formal_parameter(ast_info, i_ast , fps_ast) in
      formal_ast
    | Func ->
      accept_it();
      let i_ast = parse_identifier() in
      accept Lparen;
      let fps_ast = parse_formal_parameter_sequence() in
      accept Rparen;
      accept Colon;
      let t_ast = parse_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let formal_ast = Func_formal_parameter(ast_info, i_ast, fps_ast, t_ast) in
      formal_ast
    | _ ->
      let error_message =
        token_to_str(!current_token) ^ " cannot start a formal parameter" in
      ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
      raise (Synt_error "Syntactical error")
  )

  (** parse_actual_parameter_sequence return the Ast of Actual parameter
      sequence. *)
  and parse_actual_parameter_sequence ()  = (
    if (!current_token == Rparen) then begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actuals_ast = Empty_actual_parameter_sequence(ast_info) in
      actuals_ast
    end else begin
      let actuals_ast = parse_proper_actual_paramerer_sequence() in
      actuals_ast
    end
  )

  (** parse_actual_parameter parses the actual parameter and returns the actual
      Ast *)
  and parse_actual_parameter ()  = (
    match !current_token with
    | Identifier(a)
    | Int_literal(a)
    | Char_literal(a)
    | Operator(a) ->
      let e_ast = parse_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actual_ast = Const_actual_parameter(ast_info, e_ast) in
      actual_ast
    | Let
    | If
    | Lparen
    | Lbracket
    | Lcurly ->
      let e_ast = parse_expression() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actual_ast = Const_actual_parameter(ast_info, e_ast) in
      actual_ast
    | Var ->
      accept_it();
      let i_ast = parse_vname() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actual_ast = Var_actual_parameter(ast_info, i_ast) in
      actual_ast
    | Proc ->
      accept_it();
      let i_ast = parse_identifier() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actual_ast = Proc_actual_parameter(ast_info, i_ast) in
      actual_ast
    | Func ->
      accept_it();
      let i_ast = parse_identifier() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actual_ast = Func_actual_parameter(ast_info, i_ast) in
      actual_ast
    | _ ->
      let error_message =
        token_to_str(!current_token) ^ " cannot start an actual parameter" in
      ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
      raise (Synt_error "Syntactical error")
  )

  (** parse_proper_actual_paramerer_sequence parses the sequence of parameteres
      with parse_actual_parameter *)
  and parse_proper_actual_paramerer_sequence ()  = (
    let ap_ast = parse_actual_parameter() in
    if (!current_token == Comma) then begin
      accept_it();
      let aps_ast = parse_proper_actual_paramerer_sequence() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actuals_ast =
        Multiple_actual_parameter_sequence(ast_info, ap_ast, aps_ast) in 
      actuals_ast
    end else begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let actuals_ast = Single_actual_parameter_sequence(ast_info, ap_ast) in
      actuals_ast
    end
  )

  (** parse_type_denoter parses the type of every value, constant, variable,
      function and return the Ast *)
  and parse_type_denoter ()  = (
    match !current_token with
    | Identifier(a) ->
      let i_ast = parse_identifier() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let type_ast = Simple_type_denoter(ast_info, i_ast) in
      type_ast
    | Array ->
      accept_it();
      let il_ast = parse_integer_literal() in
      accept Of;
      let t_ast = parse_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let type_ast = Array_type_denoter(ast_info, il_ast, t_ast) in
      type_ast
    | Record ->
      accept_it();
      let f_ast = parse_field_type_denoter() in
      accept End;
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let type_ast = Record_type_denoter(ast_info, f_ast) in
      type_ast
    | _ ->
      let error_message =
        token_to_str(!current_token) ^ " cannot start a type denoter" in
      ErrorReporter.report_error error_message lb.Lexing.lex_curr_p;
      raise (Synt_error "Syntactical error")
  )

  (** parse_field_type_denoter parses a field of declaration of type denoter and
      return the Ast*)
  and parse_field_type_denoter ()  = (
    let i_ast = parse_identifier() in
    accept Colon;
    let t_ast = parse_type_denoter() in
    if (!current_token == Comma) then begin
      accept_it();
      let f_ast = parse_field_type_denoter() in
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let field_ast =
        Multiple_field_type_denoter(ast_info, i_ast , t_ast, f_ast) in 
      field_ast
    end else begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let field_ast = Single_field_type_denoter(ast_info, i_ast , t_ast) in
      field_ast
    end
  ) in

  (** Here the function become to parse the program, beginning from
      parse_command and returns the Ast program*)
  (
    current_token := (scan_func lb);
    let ast_command = parse_command() in
    if (!current_token <> Eof) then begin
      ErrorReporter.report_error "End of File expected." lb.Lexing.lex_curr_p;
      raise (Synt_error "End Of file expected");   
    end else begin
      let ast_info = {pos = lb.lex_curr_p; run = Null_runtime_entity} in
      let ast_program = Program(ast_info, ast_command) in
      ast_program
    end
  )
)