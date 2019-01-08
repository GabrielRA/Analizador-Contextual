(* ------------------------------------- *)
(* Contextual Analyzer for Caml-Triangle *)
(* Implementation file                   *)
(*                                       *)
(* (c) 2006 Luis Leopoldo Pérez.         *)
(* Last modification: March 12, 2006     *)
(* ------------------------------------- *)

open Parser
open Ast
open ErrorReporter
open IdentificationTable


(* Reports an Identifier as undeclared *)
let rec report_undeclared_identifier a = match a with
    Identifier(i,s)          -> ErrorReporter.reportError (s ^ " is not declared") i.pos
  | Checked_identifier(b,_)   -> report_undeclared_identifier b

(* Reports an operator as undeclared *)
let rec report_undeclared_operator a = match a with
    Operator(i,s)          -> ErrorReporter.reportError (s ^ " is not declared") i.pos
  | Checked_operator(b,_)   -> report_undeclared_operator b

(* Returns the spelling of an Identifier *)
let rec Identifier_name id = match id with 
    Identifier(_,s)        -> s
  | Checked_identifier(i,_) -> Identifier_name i
  
(* Returns the spelling of an operator *)
let rec operator_name id = match id with
    Operator(_,s)        -> s
  | Checked_operator(o,_) -> operator_name o


(* Obtains the type of a field Identifier *)
let rec visit_field_identifier ft id = match ft with
    Multiple_field_type_denoter(ix,i,t,mt) -> 
      if ((String.compare (Identifier_name id) (Identifier_name i)) == 0) then
        t
      else 
        visit_field_identifier mt id
  | Single_field_type_denoter(ix,i,t)      -> 
    if ((String.compare (Identifier_name id) (Identifier_name i)) == 0) then
      t
    else
      Error_type_denoter(ix)

 
(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
let rec check_program a = match a with
    Null_program  -> Null_program
  | Program(ix,b) -> Program(ix, check_command b)


(* Commands *)
and check_command a = match a with

    (* Empty command - does nothing *)
    Empty_command(_)               -> a
    
    (* Assign command - checks if LHS is a variable and if both sides have the same types *)
  | Assign_command(ix, v, e) -> 
    let vType = (check_vname v) and eType = (check_expression e) in
      (match (vType,eType) with
          (Checked_vname(_,var,_,_,t),Checked_expression(_,tt)) -> 
            if (var == false) then
              ErrorReporter.reportError "LHS of assignment is not a variable" ix.pos;
            if ((compare_types t tt) == false) then
              ErrorReporter.reportError "Assignment incompatibility" ix.pos;
              Assign_command(ix,vType,eType)
        | _                                                     -> a)
                                    
  
    (* Call command - checks if the procedure exists and if the parameters are valid *)
  | Call_command(ix, i, aps)  -> 
    let iType = (check_identifier i) in
      (match iType with Checked_identifier(_,d)  -> 
        (match !d with
            Null_declaration                                                -> report_undeclared_identifier i; a
          | Proc_declaration(ix,_,fps,_)
          | Formal_parameter_declaration(ix,Proc_formal_parameter(_,_,fps)) -> Call_command(ix,iType,(check_actual_parameter_sequence aps fps))
          | _                                                               -> ErrorReporter.reportError ((Identifier_name i) ^ " is not a procedure Identifier") ix.pos;a
        )
        | _                       -> a)
  
    (* Sequential command - checks both commands recursively *)
  | Sequential_command(ix, c1, c2) -> let c1Type = (check_command c1) and c2Type = (check_command c2) in
    Sequential_command(ix, c1Type, c2Type)
  
    (* Let (declaration) command - opens a new scope and inserts the new declaration into the identification table *)
  | Let_command(ix, d, c) -> 
      IdentificationTable.open_scope();
      let dType = (check_declaration d) and cType = (check_command c) in
        IdentificationTable.close_scope();
        Let_command(ix, dType, cType)
  
    (* If command - checks if the expression is boolean, then checks both limbs recursively *)
  | If_command(ix, e, c1, c2)      -> 
      let eType = (check_expression e) in
        (match eType with
          Checked_expression(_,Bool_type_denoter(_)) -> ()
        | _                                          -> ErrorReporter.reportError "Boolean expression expected here" ix.pos);
      let c1Type = (check_command c1) and c2Type = (check_command c2) in
        If_command(ix, eType, c1Type, c2Type)
  
    (* While command - checks if the expression is boolean, then checks the inner command recursively *)
  | While_command(ix, e, c)        -> let eType = (check_expression e) in
    (match eType with 
        Checked_expression(_,Bool_type_denoter(_)) -> ()
      | _                                          -> ErrorReporter.reportError "Boolean expression expected here" ix.pos);
                                                      let cType = (check_command c) in
                                                        While_command(ix, eType, cType)
  
(* Expressions *)
and check_expression e = match e with

    (* Empty expression - does nothing, returns a null type denoter *)
    While_command(_)                 -> Checked_expression(e, Null_type_denoter)
    
    (* Integer expression - returns an integer type denoter *)
  | Integer_expression(ix,il)        -> Checked_expression(e, check_integer_literal(il))
  
    (* Character expression - returns a character type denoter *)
  | Character_expression(ix, cl)     -> Checked_expression(e, check_character_literal(cl))
  
    (* Value-or-variable name expression - returns the variable type *)
  | Vname_expression(ix, vn)         -> let vType = (check_vname vn) in
    (match vType with
        Checked_vname(_,_,_,_,t)     -> Checked_expression(Vname_expression(ix,vType), t)
      | _                            -> e)


    (* Call expression - checks if the function exists and if the parameters are valid *)
  | Call_expression(ix, i, ap)  -> 
      let iType = (check_identifier i) in
          (match iType with
            Checked_identifier(_,d) -> 
              (match !d with
                  Null_declaration                                                 -> report_undeclared_identifier i; e
                | Func_declaration(_,_,fps,t,_)
                | Formal_parameter_declaration(_,Func_formal_parameter(_,_,fps,t)) -> Checked_expression(Call_expression(ix,iType,(check_actual_parameter_sequence ap fps)),t)
                | _                                                                -> ErrorReporter.reportError ((Identifier_name i) ^ " is not a function Identifier") ix.pos; e
              )
        | _                         -> e)
  
    (* If expression - checks if the first expression is boolean, then checks if both limbs have the same type *)
  | If_expression(ix, e1, e2, e3)  -> 
      let e1Type = (check_expression e1) and 
          e2Type = (check_expression e2) and 
          e3Type = (check_expression e3) in
            (match (e1Type, e2Type, e3Type) with 
                (Checked_expression(_,t1),Checked_expression(_,t2),Checked_expression(_,t3)) -> 
                  (match t1 with 
                    Bool_type_denoter(_)  ->
                      if ((compare_types t2 t3) == false) then 
                        ErrorReporter.reportError "Incompatible limbs in if-expression" ix.pos;
                        Checked_expression(If_expression(ix,e1Type,e2Type,e3Type),t2)
                | _                       -> ErrorReporter.reportError "Boolean expression expected here" ix.pos; e
                )
              | _                                                                          -> e)

    (* Let (declaration) expression - operns a new scope, checks the expression and inserts the new declaration into the identification table *)
  | Let_expression(ix, d, ex)  -> 
      IdentificationTable.open_scope();
      let dType = (check_declaration d) and 
          eType = (check_expression ex) in
            IdentificationTable.close_scope();
            (match eType with
                Checked_expression(_,t) -> Checked_expression(Let_expression(ix,dType,eType),t)
              | _                       -> e)

    (* Unary expression - checks if the operator exists, then checks if the operator and expression types are the same *)
  | Unary_expression(ix, o, ex)      -> 
      let eType = (check_expression ex) and 
          oType = (check_operator o) in
            (match oType with
                Checked_operator(_,d) -> 
                  (match !d with
                      Null_declaration                      -> report_undeclared_operator o; Checked_expression(e, Error_type_denoter(ix))
                    | Unary_operator_declaration(_,o,t,tr)  -> 
                        (match eType with
                            Checked_expression(_,tt)  -> 
                              if ((compare_types t tt) == false) then
                                ErrorReporter.reportError ("Wrong argument type for " ^ (operator_name o)) ix.pos;
                                Checked_expression(Unary_expression(ix,oType,eType), tr)
                          | _                         -> e
                        )
                    | _                                     -> ErrorReporter.reportError ((operator_name o) ^ " is not an unary operator") ix.pos; e
                  )
              | _                    -> e
            )
  
    (* Binary expression - checks if the operator exists, then checks if the expression and operator types are the same *)
  | Binary_expression(ix, e1, o, e2) -> 
      let e1Type = (check_expression e1) and 
          e2Type = (check_expression e2)and 
          oType  = (check_operator o) in
            (match oType with
                Checked_operator(_,d) -> 
                  (match !d with
                      Null_declaration                          -> report_undeclared_operator o; 
                                                                   Checked_expression(e, Error_type_denoter(ix))
                    | Binary_operator_declaration(_,o,t1,t2,tr) -> 
                      (match (e1Type,e2Type) with
                          (Checked_expression(_,te1),Checked_expression(_,te2)) -> 
                            (match t1 with
                                Any_type_denoter(_)  -> 
                                  if ((compare_types te1 te2) == false) then
                                    ErrorReporter.reportError ("Incompatible argument types for " ^ (operator_name o)) ix.pos
                              | _                    -> 
                                  if (((compare_types t1 te1) == false) || ((compare_types t2 te2) == false)) then
                                    ErrorReporter.reportError ("Wrong argument type for " ^ (operator_name o)) ix.pos
                            )
                        | _                                                     -> ()); 
                        Checked_expression(Binary_expression(ix,e1Type,oType,e2Type),tr)
                    | _                                         -> ErrorReporter.reportError ((operator_name o) ^ " is not a binary operator") ix.pos; e
                  )
              | _                     -> e
            )
  
  
    (* Array expression - returns an array type denoter *)
  | Array_expression(ix, aa)         -> 
      let aaType = (check_array_aggregate aa) in
        (match aaType with
            Checked_array_aggregate(Single_array_aggregate(_,Checked_expression(_,t)),i)
          | Checked_array_aggregate(Multiple_array_aggregate(_,Checked_expression(_,t),_),i)  -> 
              Checked_expression(Array_expression(ix,aaType),Array_type_denoter(ix,Integer_literal(ix,string_of_int i),t))
          | _                                                                                 -> e)
                                            
    (* Record expression - returns a record type denoter *)
  | Record_expression(ix, ra)        -> 
      let raType = (check_record_aggregate ra) in
        (match raType with
            Checked_record_aggregate(_,t)  -> Checked_expression(Record_expression(ix,raType),Record_type_denoter(ix,t))
          | _                              -> e)
                                        
    (* Already checked expression - does nothing *)
  | Checked_expression(_,_)          -> e


(* Array Aggregates *)
and check_array_aggregate a = 
  match a with
      Single_array_aggregate(ix,e)      -> Checked_array_aggregate(Single_array_aggregate(ix,(check_expression e)),1)
    
    | Multiple_array_aggregate(ix,e,aa) -> 
        let eType  = (check_expression e) and 
            aaType = (check_array_aggregate aa) in
              (match eType with
                  Checked_expression(_,t) -> 
                    (match aaType with
                        Checked_array_aggregate(Single_array_aggregate(_,Checked_expression(_,tt)), i)
                      | Checked_array_aggregate(Multiple_array_aggregate(_,Checked_expression(_,tt),_), i) -> 
                          if ((compare_types t tt) == false) then
                            ErrorReporter.reportError ("Incompatible array-aggregate element") ix.pos;
                            Checked_array_aggregate(Multiple_array_aggregate(ix, eType, aaType), i+1)
                      | _                                                                                  -> a
                    )
                | _                      -> a
              )
                                        
    | Checked_array_aggregate(_,_)       -> a


(* Record Aggregates *)
and check_record_aggregate r = 
  match r with
      Single_record_aggregate(ix,i,e)      -> 
        let eType = (check_expression e) in
          (match eType with
              Checked_expression(_,t) -> Checked_record_aggregate(Single_record_aggregate(ix,i,eType),Single_field_type_denoter(ix,i,t))
            | _                       -> r
          )                                           
    | Multiple_record_aggregate(ix,i,e,r)  -> 
        let eType = (check_expression e) and 
            rType = (check_record_aggregate r) in
              (match rType with
                  Checked_record_aggregate(_,t) -> 
                    let fType = (visit_field_identifier t i) in
                      (match fType with
                          Error_type_denoter(_) -> ()
                        | _                     -> ErrorReporter.reportError ("Duplicate field " ^ 
                                                                              (Identifier_name i) ^ 
                                                                              " in record") ix.pos);
                                                   Checked_record_aggregate(Multiple_record_aggregate(ix, i, eType, rType),
                                                                            Multiple_field_type_denoter(ix, i, 
                                                                              (match eType with 
                                                                                  Checked_expression(_,tt) -> tt 
                                                                                | _ -> Error_type_denoter(ix)), t))
                | _                           -> r)
                                                 
    | Checked_record_aggregate(_,_)        -> r

(* Value-or-variable names *)
and check_vname v = match v with

    (* Simple value or variable names *)
    Simple_vname(ix, id)       -> 
      let idType = (check_identifier id) in
        (match idType with
            Checked_identifier(_,d)  -> 
              (match !d with
                  Const_declaration(_,_,e)                                      -> Checked_vname(Simple_vname(ix,idType), false, false, 0, 
                                                                                                              (match (check_expression e) with 
                                                                                                                  Checked_expression(_,t) -> t 
                                                                                                                | _                       -> Error_type_denoter(ix)
                                                                                                              )
                                                                                                )
                | Var_declaration(_,_,t)                                        -> Checked_vname(Simple_vname(ix,idType), true, false, 0, t)
                | Formal_parameter_declaration(_,Const_formal_parameter(_,_,t)) -> Checked_vname(Simple_vname(ix,idType), false, false, 0, t)
                | Formal_parameter_declaration(_,Var_formal_parameter(_,_,t))   -> Checked_vname(Simple_vname(ix,idType), true, false, 0, t)
                | _                                                             -> ErrorReporter.reportError ((Identifier_name id) ^ " is not a const or var Identifier") ix.pos; v
              )
          | _                        -> v
        )

    (* Dot vnames - used over records *)
  | Dot_vname(ix, vn, id)      -> 
      let vType = (check_vname vn) in
        (match vType with
            Checked_vname(_,var,_,_,t)  -> 
              (match t with
                  Record_type_denoter(_,ft)  -> 
                    let fType = (visit_field_identifier ft id) in
                      (match fType with
                          Error_type_denoter(_)  -> 
                            ErrorReporter.reportError ("No field " ^ (Identifier_name id) ^ " in this record type") ix.pos; v
                        | _                      -> 
                            Checked_vname(Dot_vname(ix,vType,(check_identifier id)), var, false, 0,fType)
                      )
                | _                          -> ErrorReporter.reportError "Record expected here" ix.pos; v
              )
          | _                           -> v
        )

    (* Subscript vnames - used over arrays *)
  | Subscript_vname(ix, vn, e) -> 
      let vType = (check_vname vn) and 
          eType = (check_expression e) in
            (match vType with
                Checked_vname(_,var,_,_,t) -> 
                  (match t with
                      Array_type_denoter(_,_,tt)  -> 
                        (match eType with
                            Checked_expression(ex, Int_type_denoter(_))  -> 
                              Checked_vname(Subscript_vname(ix,vType,eType), var, false, 0, tt)
                          | _                                            -> 
                            ErrorReporter.reportError "Integer expression expected here" ix.pos; v
                        )
                    | _ -> ErrorReporter.reportError "Array expected here" ix.pos; v
                  )
              | _ -> v
            )

    (* Already checked vnames - do nothing *)
  | Checked_vname(_,_,_,_,_) -> v


(* Declarations *)
and check_declaration d = match d with
    Null_declaration -> d
    
  | Const_declaration(ix,i,e) -> 
    let eType = (check_expression e) in
      if (IdentificationTable.exists (Identifier_name i)) then
        ErrorReporter.reportError ("Identifier " ^ (Identifier_name i) ^ " already declared") ix.pos;
        IdentificationTable.enter (Identifier_name i) (ref (Const_declaration(ix,i,eType)));
        !(IdentificationTable.retrieve (Identifier_name i))
  
  | Var_declaration(ix,i,t) -> 
      let tType = (check_type_denoter t) in
        if (IdentificationTable.exists (Identifier_name i)) then
          ErrorReporter.reportError ("Identifier " ^ (Identifier_name i) ^ " already declared") ix.pos;
          IdentificationTable.enter (Identifier_name i) (ref (Var_declaration(ix,i,tType)));
          !(IdentificationTable.retrieve (Identifier_name i))
  

  | Proc_declaration(ix,i,fps,c) ->
      if (IdentificationTable.exists (Identifier_name i)) then
        ErrorReporter.reportError ("Identifier " ^ (Identifier_name i) ^ " already declared") ix.pos;
        IdentificationTable.enter (Identifier_name i) (ref (Proc_declaration(ix,i,fps,c)));
        let elem = IdentificationTable.retrieve_element (Identifier_name i) in
          IdentificationTable.open_scope();
          let fpsType = (check_formal_parameter_sequence fps) in
            elem.attr <- (ref (Proc_declaration(ix,i,fpsType,c)));
            let cType = (check_command c) in
              IdentificationTable.close_scope();
              elem.attr <- (ref (Proc_declaration(ix,i,fpsType,cType)));
              !(IdentificationTable.retrieve (Identifier_name i))
                                                    
  | Func_declaration(ix,i,fps,t,e) -> 
      let tType = (check_type_denoter t) in
        if (IdentificationTable.exists (Identifier_name i)) then
          ErrorReporter.reportError ("Identifier " ^ (Identifier_name i) ^ " already declared") ix.pos;
          IdentificationTable.enter (Identifier_name i) (ref (Func_declaration(ix,i,fps,tType,e)));
          let elem = IdentificationTable.retrieve_element (Identifier_name i) in
            IdentificationTable.open_scope();
            let fpsType = (check_formal_parameter_sequence fps) in
              elem.attr <- (ref (Func_declaration(ix,i,fpsType,tType,e)));
              let eType = (check_expression e) in
                IdentificationTable.close_scope();
                elem.attr <- (ref (Func_declaration(ix,i,fpsType,tType,eType)));
                (match eType with
                    Checked_expression(_,t) -> 
                      if (t != tType) then
                        ErrorReporter.reportError ("Body of function " ^ (Identifier_name i) ^ " has wrong type") ix.pos
                  | _ -> ()
                ); 
                !(IdentificationTable.retrieve (Identifier_name i))

  
  | Type_declaration(ix,i,t) -> let tType = (check_type_denoter t) in
                                                    if (IdentificationTable.exists (Identifier_name i)) then
                                                       ErrorReporter.reportError ("Identifier " ^ (Identifier_name i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (Identifier_name i) (ref (Type_declaration(ix,i,tType)));
                                                    !(IdentificationTable.retrieve (Identifier_name i))

  | Unary_operator_declaration(ix,o,t1,t2) -> d
  
  | Binary_operator_declaration(ix,o,t1,t2,t3) -> d
  
  | Formal_parameter_declaration(_,_) -> d
  
  | Sequential_declaration(ix,d1,d2) -> 
      let d1Type = (check_declaration d1) and 
          d2Type = (check_declaration d2) in
            Sequential_declaration(ix, d1Type, d2Type)


(* Actual Parameters *)
and check_actual_parameter a f = match a with
      Const_actual_parameter(ix,e) -> 
        let eType = (check_expression e) in
        (match f with
            Const_formal_parameter(_,_,t) -> 
              (match eType with
                  Checked_expression(_,tt) -> 
                    if ((compare_types t tt) == false) then
                      begin
                        ErrorReporter.reportError "Wrong type for const actual parameter" ix.pos;
                        a
                      end
                    else
                      Const_actual_parameter(ix,eType)
                | _ -> a)
          | _ -> ErrorReporter.reportError "Const actual parameter not expected here" ix.pos; a
        )

  | Var_actual_parameter(ix,v) -> 
      let vType = (check_vname v) in
        (match vType with
            Checked_vname(_,false,_,_,_) -> ErrorReporter.reportError "Actual parameter is not a variable" ix.pos; a
          | Checked_vname(_,true,_,_,tt) -> 
              (match f with
                  Var_formal_parameter(_,_,t) -> 
                      if ((compare_types t tt) == false) then
                        begin
                          ErrorReporter.reportError "Wrong type for var actual parameter" ix.pos; a
                        end
                      else
                        Var_actual_parameter(ix,vType)
                | _ -> ErrorReporter.reportError "Var actual parameter not expected here" ix.pos; a
              )
          | _ -> a
        )


  | Proc_actual_parameter(ix,id) -> 
      let idType = (check_identifier id) in
        (match f with
            Proc_formal_parameter(_,_,fp) -> 
              (match idType with
                  Checked_identifier(i,d) -> 
                      (match !d with
                          Null_declaration -> report_undeclared_identifier(i); a
                        | Formal_parameter_declaration(_,Proc_formal_parameter(_,i,fps))
                        | Proc_declaration(_,i,fps,_) -> 
                            if ((compare_fps fp fps) == false) then
                              ErrorReporter.reportError ("Wrong signature for procedure " ^ (Identifier_name i)) ix.pos;
                            Proc_actual_parameter(ix,idType)
                        | _ -> ErrorReporter.reportError ((Identifier_name i) ^ " is not a procedure Identifier") ix.pos; a
                      )
                | _ -> a
              )
          | _ -> ErrorReporter.reportError "Proc actual parameter not expected here" ix.pos; a
        )
                                       
  
  | Func_actual_parameter(ix,id) -> 
      let idType = (check_identifier id) in
        (match f with
            Func_formal_parameter(_,_,fp,t) -> 
              (match idType with
                  Checked_identifier(i,d) -> 
                    (match !d with
                        Null_declaration -> report_undeclared_identifier(i); a
                      | Formal_parameter_declaration(_,Func_formal_parameter(_,i,fps,tp))
                      | Func_declaration(_,i,fps,tp,_) -> 
                          if ((compare_fps fp fps) == false) then
                            ErrorReporter.reportError ("Wrong signature for function " ^ (Identifier_name i)) ix.pos
                          else if ((compare_types t tp) == false) then
                            ErrorReporter.reportError ("Wrong type for function " ^ (Identifier_name i)) ix.pos;
                            Func_actual_parameter(ix,idType)
                      | _ -> ErrorReporter.reportError ((Identifier_name i) ^ " is not a function Identifier") ix.pos; a
                    )
                | _ -> a
              )
          | _ -> ErrorReporter.reportError "Func actual parameter not expected here" ix.pos; a
        )


(* Actual Parameter Sequences *)
and check_actual_parameter_sequence a f = match a with
    Empty_actual_parameter_sequence(ix) -> 
      (match f with 
          Empty_formal_parameter_sequence(_) -> a
        | _ -> ErrorReporter.reportError "Too few actual parameters" ix.pos; a
      )
                                                                                       
  | SingleActualParameterSequence(ix,b) -> 
      (match f with
          Single_formal_parameter_sequence(ixx,fp) -> SingleActualParameterSequence(ixx, (check_actual_parameter b fp))
        | _ -> ErrorReporter.reportError "Incorrect number of actual parameters" ix.pos; a
      )
                                                                                           
  | MultipleActualParameterSequence(ix,b,c) -> 
      (match f with
          Multiple_formal_parameter_sequence(ixx,fp,fps) -> 
            let fpType = (check_actual_parameter b fp) and 
                fpsType = (check_actual_parameter_sequence c fps) in 
                  MultipleActualParameterSequence(ixx, fpType, fpsType)
        | _ -> ErrorReporter.reportError "Too many actual parameters" ix.pos; a
      )


(* Formal Parameters *)
and checkFormalParameter f = match f with
    Const_formal_parameter(ix,i,t) -> 
      let tType = (check_type_denoter t) in
        if (IdentificationTable.exists (Identifier_name i)) then
          ErrorReporter.reportError ("Duplicated formal parameter " ^ (Identifier_name i)) ix.pos;
        let cfp = Const_formal_parameter(ix,i,tType) in
          IdentificationTable.enter (Identifier_name i) (ref (Formal_parameter_declaration(ix,cfp)));
          cfp
                                                                                  
  | Var_formal_parameter(ix,i,t) -> 
      let tType = (check_type_denoter t) in
        if (IdentificationTable.exists (Identifier_name i)) then
          ErrorReporter.reportError ("Duplicated formal parameter " ^ (Identifier_name i)) ix.pos;
        let vfp = Var_formal_parameter(ix,i,tType) in
          IdentificationTable.enter (Identifier_name i) (ref (Formal_parameter_declaration(ix,vfp)));
          vfp
                                       
  | Proc_formal_parameter(ix,i,fps) -> IdentificationTable.open_scope();
      let fpsType = (check_formal_parameter_sequence fps) in
        IdentificationTable.close_scope();
        if (IdentificationTable.exists (Identifier_name i)) then
          ErrorReporter.reportError ("Duplicated formal parameter " ^ (Identifier_name i)) ix.pos;
        let pfp = Proc_formal_parameter(ix,i,fpsType) in
          IdentificationTable.enter (Identifier_name i) (ref (Formal_parameter_declaration(ix,pfp)));
          pfp

  | Func_formal_parameter(ix,i,fps,t) -> 
      IdentificationTable.open_scope();
      let fpsType = (check_formal_parameter_sequence fps)
      and tType   = (check_type_denoter t) in
          IdentificationTable.close_scope();
      if (IdentificationTable.exists (Identifier_name i)) then
          ErrorReporter.reportError ("Duplicated formal parameter " ^ (Identifier_name i)) ix.pos;
      let ffp = Func_formal_parameter(ix,i,fpsType,tType) in
          IdentificationTable.enter (Identifier_name i) (ref (Formal_parameter_declaration(ix,ffp)));
          ffp

(* Formal Parameter Sequences *)
and check_formal_parameter_sequence f = match f with
    Empty_formal_parameter_sequence(_) -> f
    
  | Single_formal_parameter_sequence(ix,fp) -> Single_formal_parameter_sequence(ix, checkFormalParameter fp)
  
  | Multiple_formal_parameter_sequence(ix,fp,fps) -> let fpType = (checkFormalParameter fp)
                                                  and fpsType = (check_formal_parameter_sequence fps) in
                                                      Multiple_formal_parameter_sequence(ix, fpType, fpsType)

(* Type Denoters *)
and check_type_denoter a = match a with
    Null_type_denoter -> a
    
  | Error_type_denoter(_) -> a
  
  | Any_type_denoter(_) -> a
  
  | Simple_type_denoter(ix,i) -> let b = (check_identifier i) in
                                  (match b with
                                      Checked_identifier(_,a) -> 
                                        (match !a with
                                            Null_declaration -> report_undeclared_identifier b;
                                                                Error_type_denoter(ix)
                                          | Type_declaration(_,_,t) -> t
                                          | _                       -> ErrorReporter.reportError ((Identifier_name i) ^ " is not a type Identifier") ix.pos;
                                                                       Error_type_denoter(ix))
                                    | _ -> Error_type_denoter(ix)
                                  )
                                      
  | Array_type_denoter(ix,il,t) ->  
      if ((int_of_string (match il with Integer_literal(_,s) -> s)) == 0) then
                              ErrorReporter.reportError "Arrays must not be empty" (match il with Integer_literal(is,_) -> is).pos;
                              Array_type_denoter(ix,il,check_type_denoter(t))
                                 
  | Record_type_denoter(ix,ft) -> Record_type_denoter(ix, check_field_type_denoter(ft))
  
  | Bool_type_denoter(_) -> a
  
  | Int_type_denoter(_) -> a
  
  | Char_type_denoter(_) -> a


(* Field Type Denoters *)
and check_field_type_denoter a = match a with
    Single_field_type_denoter(ix,i,t) -> 
      Single_field_type_denoter(ix, i, (check_type_denoter t))
    
  | Multiple_field_type_denoter(ix,i,t,ft) -> 
      let tType  = (check_type_denoter t) and 
          ftType = (check_field_type_denoter ft) in 
            Multiple_field_type_denoter(ix, i, tType, ftType)
  
(* Integer Literals *)
and check_integer_literal a = match a with
    Integer_literal(i,_) -> 
      let dx = (IdentificationTable.retrieve "Integer") in
        (match !dx with
            Type_declaration(_,_,t) -> t
          | _ -> Int_type_denoter(i))

(* Character Literals *)
and check_character_literal a = match a with
    Character_literal(i,_) -> 
      let dx = (IdentificationTable.retrieve "Char") in
        (match !dx with
            Type_declaration(_,_,t) -> t
          | _ -> Char_type_denoter(i))

(* Identifiers *)
and check_identifier a = match a with
    Identifier(_,s) -> 
      Checked_identifier(a, (IdentificationTable.retrieve s))
    
  | Checked_identifier(_,_) -> a


(* Operators *)  
and check_operator a = match a with
    Operator(_,s) -> 
      Checked_operator(a, (IdentificationTable.retrieve s))
    
  | Checked_operator(_,_) -> a

(* Checks if two types are equivalent *)  
and compare_types t1 t2 = match (t1,t2) with
    (Int_type_denoter(_),Int_type_denoter(_))
  | (Char_type_denoter(_),Char_type_denoter(_))
  | (Any_type_denoter(_),Any_type_denoter(_))
  | (Bool_type_denoter(_),Bool_type_denoter(_)) -> true
  
  | (Simple_type_denoter(_,i1),Simple_type_denoter(_,i2)) -> 
      (match ((check_identifier i1),(check_identifier i2)) with
          (Checked_identifier(_,d1),Checked_identifier(_,d2)) -> 
            (match (!d1,!d2) with
                (Type_declaration(_,_,tx1),Type_declaration(_,_,tx2)) -> (compare_types tx1 tx2)
              | _ -> false
            )
        | _ -> false
      )
  | (Array_type_denoter(_,il1,tx1),Array_type_denoter(_,il2,tx2)) -> 
      let size =  
        (match (il1,il2) with (Integer_literal(_,s1),Integer_literal(_,s2)) -> 
          ((String.compare s1 s2) == 0)) in
            (size && (compare_types tx1 tx2))
                                                                   
  | (Record_type_denoter(_,ft1),Record_type_denoter(_,ft2)) -> 
      let rec compareFTs f1 f2 = 
        (match (f1,f2) with
            (Single_field_type_denoter(_,i1,tx1),Single_field_type_denoter(_,i2,tx2)) -> 
              (compare_types tx1 tx2) && ((String.compare (Identifier_name i1) (Identifier_name i2)) == 0)
          | (Multiple_field_type_denoter(_,i1,tx1,ftx1),Multiple_field_type_denoter(_,i2,tx2,ftx2)) -> 
              (compare_types tx1 tx2) && ((String.compare (Identifier_name i1) (Identifier_name i2)) == 0) && (compareFTs ftx1 ftx2)
          | _ -> false
        ) in
          (compareFTs ft1 ft2)
                                                               
  | _ -> false
  
(* Compares two formal parameters and its types *)
and compare_fp fp1 fp2 = match (fp1,fp2) with
    (Const_formal_parameter(_,_,t1),Const_formal_parameter(_,_,t2))
  | (Var_formal_parameter(_,_,t1),Var_formal_parameter(_,_,t2)) -> (compare_types t1 t2)
  | (Proc_formal_parameter(_,_,fps1),Proc_formal_parameter(_,_,fps2)) -> (compare_fps fps1 fps2)
  | (Func_formal_parameter(_,_,fps1,t1),Func_formal_parameter(_,_,fps2,t2)) -> (compare_fps fps1 fps2) && (compare_types t1 t2)
  | _ -> false

(* Compares two formal parameter sequences and its types *)  
and compare_fps fps1 fps2 = match (fps1,fps2) with
    (Empty_formal_parameter_sequence(_),Empty_formal_parameter_sequence(_)) -> true
  | (Single_formal_parameter_sequence(_,fp1),Single_formal_parameter_sequence(_,fp2)) -> (compare_fp fp1 fp2)
  | (Multiple_formal_parameter_sequence(_,fp1,fps1),Multiple_formal_parameter_sequence(_,fp2,fps2)) -> (compare_fp fp1 fp2) && (compare_fps fps1 fps2)
  | _ -> false
  