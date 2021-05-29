open Ast
open LilyFront.Error

type scope_kind = 
    | ScopeKindFunction of ast_kind
    | ScopeKindLambda of ast_kind
    | ScopeKindModule of ast_kind
    | ScopeKindClass of ast_kind

type scope_location = {
    file: string;
    kind: scope_kind;
}

val new_scope_location : string -> scope_kind -> scope_location
val ast_to_scope : ast_kind -> (scope_kind, error_id) result

module FindInScope : sig
    val find_in_function : scope_kind -> unit
    val find_in_lambda : scope_kind -> unit
    val find_in_module : scope_kind -> unit
    val find_in_class : scope_kind -> unit
    val find_in_global : scope_kind -> unit
end
