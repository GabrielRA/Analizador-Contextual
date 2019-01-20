open String
open Ast
open Id_entry

val write_close_scope_declarations:int -> id_entry list ->  unit
val write_open_scope_declarations:int -> unit
val write_file: string -> unit