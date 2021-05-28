open Ast

type scope_kind = 
    | ScopeKindFunction of ast_kind
    | ScopeKindLambda of ast_kind
    | ScopeKindModule of ast_kind
    | ScopeKindClass of ast_kind

type scope_location = {
    file: string;
    kind: scope_kind;
}

let new_scope_location file kind = {
    file = file;
    kind = kind;
}
