open Ast
open RuntimeEntity


type id_entry = {mutable id: string; mutable attr: ast_declaration ref; mutable levl: int}