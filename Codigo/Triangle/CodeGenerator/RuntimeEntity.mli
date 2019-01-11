(* --------------------------------------------- *)
(* Runtime Entities Definition for Caml-Triangle *)
(* Interface file                                *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: May 26, 2006               *)
(* --------------------------------------------- *)


type object_address = { level: int; displacement: int }

                                           
type runtime_entity = Null_runtime_entity     
                   | Known_value         of int*int (* size,value *)
                   | Unknown_value       of int*object_address (* size *)
                   | Known_address       of int*object_address (* size *)
                   | Unknown_address     of int*object_address (* size *)
                   | Known_routine       of int*object_address (* size *)
                   | Unknown_routine     of int*object_address (* size *)
                   | Primitive_routine   of int*int (* size, displacement *)
                   | Equality_routine    of int*int (* size, displacement *)
                   | Field              of int*int (* size, fieldOffset *)
                   | Type_representation of int     (* size *)