(*
This program was made by Luis Leopoldo Pérez at March 12, 2006.
This program was repaired, completed, verified and validated by students
of ITCR at 2018.
Identification Table Module for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Christian León Guevara
@author Gabriel Ramírez Ramírez

Last modification: January, 2019
*)

open Ast
open RuntimeEntity

type id_entry = {mutable id: string; mutable attr: ast_declaration ref; mutable levl: int}

type id_list = 
  Nullid_list
| Id_list of id_entry list            

let level = ref 0

let identifier_list = ref Nullid_list

let unboxid_list t = 
  match t with
    Nullid_list -> []
  | Id_list s -> s

let open_scope() = incr(level)

let close_scope() =
  identifier_list := 
    Id_list(
	  List.filter (fun x -> (x.levl != !level)) 
	  (unboxid_list !identifier_list)
    );
  decr(level)
    (* Invocacion del generador de Xml del level actual id_list(List.filter (fun x -> (x.levl = !level)) (unboxid_list !identifier_list)) *)     
    

let exists new_id =
  (List.exists (fun x -> 
	(((String.compare x.id new_id) == 0) && (x.levl == !level))) 
	(unboxid_list !identifier_list))
    
let enter new_id new_decl = 
  let new_entry = {id=new_id; attr=new_decl; levl=(!level)} in
    identifier_list := Id_list([new_entry] @ unboxid_list(!identifier_list))      

let retrieve old_Id =
  try
    (List.find 
	  (fun x -> ((String.compare x.id old_Id) == 0)) 
	  (unboxid_list !identifier_list)
	).attr
    with Not_found -> ref Null_declaration
    
let retrieve_element old_Id =
  try
    (List.find 
      (fun x -> ((String.compare x.id old_Id) == 0) && (x.levl == !level)) 
	  (unboxid_list !identifier_list)
	)
  with Not_found -> {id="";attr=ref Null_declaration;levl=0}

    
(* Standard Environment *)

let boolean_type = 
  Bool_type_denoter({pos = Lexing.dummy_pos; run = Null_runtime_entity})
let char_type = 
  Char_type_denoter({pos = Lexing.dummy_pos; run = Null_runtime_entity})
let integer_type = 
  Int_type_denoter({pos = Lexing.dummy_pos; run = Null_runtime_entity})
let any_type =
  Any_type_denoter({pos = Lexing.dummy_pos; run = Null_runtime_entity})
let error_type = 
  Error_type_denoter({pos = Lexing.dummy_pos; run = Null_runtime_entity})

let boolean_decl = 
  Type_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "Boolean"),
	boolean_type
  )
  
let char_decl = 
  Type_declaration(
    {pos = Lexing.dummy_pos;run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos;run = Null_runtime_entity}, "Char"),
	char_type
  )

let integer_decl = 
  Type_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "Integer"), 
	integer_type
  )

let false_decl = 
  Const_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "false"),
	Checked_expression(
	  Empty_expression({pos = Lexing.dummy_pos; run = Null_runtime_entity}),
	  boolean_type
	)
  )
  
let true_decl = 
  Const_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos;run = Null_runtime_entity}, "true"),
	Checked_expression(
	  Empty_expression({pos = Lexing.dummy_pos; run = Null_runtime_entity}),
	  boolean_type
	)
  )
  
let maxint_decl = 
  Const_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "maxint"), 
    Checked_expression(
	  Empty_expression({pos = Lexing.dummy_pos;run = Null_runtime_entity}),
	  integer_type
	)
  )

let not_decl = 
  Unary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "\\"),
	boolean_type, 
	boolean_type
  )

let and_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "/\\"),
	boolean_type,
	boolean_type,
	boolean_type
  )
  
let or_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "\\/"),
	boolean_type,
	boolean_type,
	boolean_type
  )

let add_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "+"),
	integer_type,
	integer_type,
	integer_type
  )
  
let substractDecl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "-"),
	integer_type,
	integer_type, 
	integer_type
  )
  
let multiply_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "*"), 
	integer_type,
	integer_type,
	integer_type
  )
  
let divide_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "/"),
	integer_type, 
	integer_type, 
	integer_type
  )

let modulo_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "//"), 
    integer_type,
	integer_type,
	integer_type
  )
  
let equal_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "="), 
	any_type,
	any_type,
  	boolean_type
  )
  
let unequal_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "\\="), 
	any_type, 
	any_type, 
	boolean_type
  )

let less_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "<"), 
	integer_type,
  	integer_type, 
	boolean_type
  )

let notless_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ">="), 
  	integer_type,
	integer_type, 
  	boolean_type
  )

let greater_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ">"), 
	integer_type,
	integer_type, 
	boolean_type
  )

let notgreater_decl = 
  Binary_operator_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Operator({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "<="), 
	integer_type, 
	integer_type, 
	boolean_type
  )

let get_decl = 
  Proc_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "get"), 
  	Single_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	  Var_formal_parameter(
	    {pos = Lexing.dummy_pos; run = Null_runtime_entity},
  	    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ""),
		char_type
	    )
	  ),
  	Empty_command({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )
  
let put_decl = 
  Proc_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "put"), 
  	Single_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	  Const_formal_parameter(
	    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ""), 
		char_type
	  )
	), 
  	Empty_command({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )
    
let getint_decl = 
  Proc_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "getint"), 
  	Single_formal_parameter_sequence(
      {pos = Lexing.dummy_pos; run = Null_runtime_entity},
  	  Var_formal_parameter(
	    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ""), 
		integer_type
	  )
	),
  	Empty_command({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let putint_decl = 
  Proc_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "putint"), 
  	Single_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity},
  	  Const_formal_parameter(
	    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ""),
		integer_type
	  )
	),
  	Empty_command({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let geteol_decl = 
  Proc_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "geteol"),
  	Empty_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}
	), 
  	Empty_command({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let puteol_decl = 
  Proc_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "puteol"), 
  	Empty_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}
	), 
  	Empty_command({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let chr_decl = 
  Func_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "chr"), 
    Single_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity},
	  Const_formal_parameter(
	    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
	    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ""),
	    integer_type
	  )
	), 
	char_type,
    Empty_expression({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let ord_decl = 
  Func_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "ord"), 
  	Single_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	  Const_formal_parameter(
	    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
  	    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, ""),
	    char_type
	  )
	),
	integer_type, 
  	Empty_expression({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let eol_decl = 
  Func_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "eol"), 
  	Empty_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}
	),
	boolean_type, 
  	Empty_expression({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let eof_decl = 
  Func_declaration(
    {pos = Lexing.dummy_pos; run = Null_runtime_entity}, 
    Identifier({pos = Lexing.dummy_pos; run = Null_runtime_entity}, "eof"), 
  	Empty_formal_parameter_sequence(
	  {pos = Lexing.dummy_pos; run = Null_runtime_entity}
	), 
	boolean_type, 
  	Empty_expression({pos = Lexing.dummy_pos; run = Null_runtime_entity})
  )

let _ =
  (enter "Boolean" (ref boolean_decl));
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