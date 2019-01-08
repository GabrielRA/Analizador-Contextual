(* --------------------------------------------- *)
(* Identification Table Module for Caml-Triangle *)
(* Implementation file                           *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: April 21, 2006             *)
(* --------------------------------------------- *)

open Ast
open RuntimeEntity

type id_entry = {mutable id: string; mutable attr: astDeclaration ref; mutable levl: int}

type id_list = Nullid_list
  | id_list of id_entry list            

let level = ref 0

let identifier_list = ref Nullid_list

let unboxid_list t = match t with
    Nullid_list -> []
  | id_list s -> s

let open_scope() = incr(level)

let close_scope() =     
    identifier_list := id_list(List.filter (fun x -> (x.levl != !level)) (unboxid_list !identifier_list));
    decr(level)

let exists new_id =
    (List.exists (fun x -> (((String.compare x.id new_id) == 0) && (x.levl == !level))) (unboxid_list !identifier_list))
    
let enter new_id newDecl = 
    let new_entry = {id=new_id; attr=newDecl; levl=(!level)} in
    identifier_list := id_list([new_entry] @ unboxid_list(!identifier_list))      

let retrieve old_Id =
    try
       (List.find (fun x -> ((String.compare x.id old_Id) == 0)) (unboxid_list !identifier_list)).attr
    with Not_found -> ref Null_declaration
    
let retrieve_element old_Id =
    try
       (List.find (fun x -> ((String.compare x.id old_Id) == 0) && (x.levl == !level)) (unboxid_list !identifier_list))
    with Not_found -> {id="";attr=ref Null_declaration;levl=0}

    
(* Standard Environment *)

let boolean_type = Bool_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let char_type    = Char_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let integer_type = Int_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let any_type     = Any_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let error_type   = Error_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})

let boolean_decl = 
  Type_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Boolean"), boolean_type)
  
let char_decl = 
  Type_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Char"), char_type)

  let integer_decl = 
  Type_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Integer"), integer_type)

let false_decl = 
  Const_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "false"), Checked_expression(Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), boolean_type))
  
let true_decl = 
  Const_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "true"), Checked_expression(Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), boolean_type))
  
let maxint_decl = 
  Const_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "maxint"), Checked_expression(Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), integer_type))

let not_decl = 
  Unary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\"), boolean_type, boolean_type)

let and_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "/\\"), boolean_type, boolean_type, boolean_type)
  
let or_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\/"), boolean_type, boolean_type, boolean_type)

let add_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "+"), integer_type, integer_type, integer_type)
  
let substractDecl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "-"), integer_type, integer_type, integer_type)
  
let multiply_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "*"), integer_type, integer_type, integer_type)
  
let divide_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "/"), integer_type, integer_type, integer_type)

let modulo_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "//"), integer_type, integer_type, integer_type)
  
let equal_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "="), any_type, any_type, boolean_type)
  
let unequal_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\="), any_type, any_type, boolean_type)

let less_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "<"), integer_type, integer_type, boolean_type)

let notless_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ">="), integer_type, integer_type, boolean_type)

let greater_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ">"), integer_type, integer_type, boolean_type)

let notgreater_decl = 
  Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "<="), integer_type, integer_type, boolean_type)


let get_decl = 
  Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "get"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Var_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), char_type)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
  
let put_decl = 
  Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "put"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), char_type)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
  
let getint_decl = 
  Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "getint"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Var_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integer_type)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let putint_decl = 
  Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "putint"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integer_type)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let geteol_decl = 
  Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "geteol"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let puteol_decl = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "puteol"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let chr_decl = 
  Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "chr"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integer_type)), char_type, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let ord_decl = 
  Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "ord"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), char_type)), integer_type, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let eol_decl = 
  Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "eol"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), boolean_type, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let eof_decl = 
  Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "eof"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), boolean_type, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let _ = (enter "Boolean" (ref boolean_decl));
        (enter "Char" (ref char_decl));
        (enter "Integer" (ref integer_decl));
        (enter "false" (ref false_decl));
        (enter "true" (ref true_decl));
        (enter "maxint" (ref maxint_decl));
        (enter "\\" (ref not_decl));
        (enter "/\\" (ref and_decl));
        (enter "\\/" (ref or_decl));
        (enter "+" (ref add_decl));
        (enter "-" (ref substractDecl));
        (enter "*" (ref multiply_decl));
        (enter "/" (ref divide_decl));
        (enter "//" (ref modulo_decl));
        (enter "=" (ref equal_decl));
        (enter "\\=" (ref unequal_decl));
        (enter "<" (ref less_decl));
        (enter ">=" (ref notless_decl));
        (enter ">" (ref greater_decl));
        (enter "<=" (ref notgreater_decl));
        (enter "get" (ref get_decl));
        (enter "put" (ref put_decl));
        (enter "getint" (ref getint_decl));
        (enter "putint" (ref putint_decl));
        (enter "geteol" (ref geteol_decl));
        (enter "puteol" (ref puteol_decl));
        (enter "chr" (ref chr_decl));
        (enter "ord" (ref ord_decl));
        (enter "eol" (ref eol_decl));
        (enter "eof" (ref eof_decl))
