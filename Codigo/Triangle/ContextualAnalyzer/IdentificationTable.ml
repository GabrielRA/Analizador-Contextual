(* --------------------------------------------- *)
(* Identification Table Module for Caml-Triangle *)
(* Implementation file                           *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: April 21, 2006             *)
(* --------------------------------------------- *)

open Ast
open RuntimeEntity

type idEntry = {mutable id: string; mutable attr: astDeclaration ref; mutable levl: int}

type idList = NullIdList
            | IdList of idEntry list
            

let level = ref 0

let IdentifierList = ref NullIdList

let unboxIdList t = match t with
    NullIdList -> []
  | IdList s -> s

let openScope() = incr(level)

let closeScope() =     
    IdentifierList := IdList(List.filter (fun x -> (x.levl != !level)) (unboxIdList !IdentifierList));
    decr(level)

let exists newId =
    (List.exists (fun x -> (((String.compare x.id newId) == 0) && (x.levl == !level))) (unboxIdList !IdentifierList))
    
let enter newId newDecl = 
    let newEntry = {id=newId; attr=newDecl; levl=(!level)} in
    IdentifierList := IdList([newEntry] @ unboxIdList(!IdentifierList))      

let retrieve oldId =
    try
       (List.find (fun x -> ((String.compare x.id oldId) == 0)) (unboxIdList !IdentifierList)).attr
    with Not_found -> ref Null_declaration
    
let retrieveElement oldId =
    try
       (List.find (fun x -> ((String.compare x.id oldId) == 0) && (x.levl == !level)) (unboxIdList !IdentifierList))
    with Not_found -> {id="";attr=ref Null_declaration;levl=0}

    
(* Standard Environment *)

let booleanType = Bool_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let charType    = Char_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let integerType = Int_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let anyType     = Any_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let errorType   = Error_type_denoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})

let booleanDecl = Type_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Boolean"), booleanType)
let charDecl    = Type_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Char"), charType)
let integerDecl = Type_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Integer"), integerType)

let falseDecl  = Const_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "false"), Checked_expression(Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType))
let trueDecl   = Const_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "true"), Checked_expression(Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType))
let maxintDecl = Const_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "maxint"), Checked_expression(Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), integerType))

let notDecl = Unary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\"), booleanType, booleanType)

let andDecl        = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "/\\"), booleanType, booleanType, booleanType)
let orDecl         = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\/"), booleanType, booleanType, booleanType)
let addDecl        = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "+"), integerType, integerType, integerType)
let substractDecl  = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "-"), integerType, integerType, integerType)
let multiplyDecl   = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "*"), integerType, integerType, integerType)
let divideDecl     = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "/"), integerType, integerType, integerType)
let moduloDecl     = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "//"), integerType, integerType, integerType)
let equalDecl      = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "="), anyType, anyType, booleanType)
let unequalDecl    = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\="), anyType, anyType, booleanType)
let lessDecl       = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "<"), integerType, integerType, booleanType)
let notlessDecl    = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ">="), integerType, integerType, booleanType)
let greaterDecl    = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ">"), integerType, integerType, booleanType)
let notgreaterDecl = Binary_operator_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "<="), integerType, integerType, booleanType)

let getDecl    = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "get"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Var_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), charType)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let putDecl    = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "put"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), charType)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let getintDecl = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "getint"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Var_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integerType)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let putintDecl = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "putint"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integerType)), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let geteolDecl = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "geteol"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let puteolDecl = Proc_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "puteol"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), Empty_command({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let chrDecl = Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "chr"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integerType)), charType, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let ordDecl = Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "ord"), Single_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Const_formal_parameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), charType)), integerType, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let eolDecl = Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "eol"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let eofDecl = Func_declaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "eof"), Empty_formal_parameter_sequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType, Empty_expression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let _ = (enter "Boolean" (ref booleanDecl));
        (enter "Char" (ref charDecl));
        (enter "Integer" (ref integerDecl));
        (enter "false" (ref falseDecl));
        (enter "true" (ref trueDecl));
        (enter "maxint" (ref maxintDecl));
        (enter "\\" (ref notDecl));
        (enter "/\\" (ref andDecl));
        (enter "\\/" (ref orDecl));
        (enter "+" (ref addDecl));
        (enter "-" (ref substractDecl));
        (enter "*" (ref multiplyDecl));
        (enter "/" (ref divideDecl));
        (enter "//" (ref moduloDecl));
        (enter "=" (ref equalDecl));
        (enter "\\=" (ref unequalDecl));
        (enter "<" (ref lessDecl));
        (enter ">=" (ref notlessDecl));
        (enter ">" (ref greaterDecl));
        (enter "<=" (ref notgreaterDecl));
        (enter "get" (ref getDecl));
        (enter "put" (ref putDecl));
        (enter "getint" (ref getintDecl));
        (enter "putint" (ref putintDecl));
        (enter "geteol" (ref geteolDecl));
        (enter "puteol" (ref puteolDecl));
        (enter "chr" (ref chrDecl));
        (enter "ord" (ref ordDecl));
        (enter "eol" (ref eolDecl));
        (enter "eof" (ref eofDecl))
