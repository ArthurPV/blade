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

let new_scope_location file kind = {
    file = file;
    kind = kind;
}

let ast_to_scope s_kind = 
    match s_kind with
    | Expr (ExprFunDeclare {id;
                            visibility;
                            args;
                            body}) -> Ok (ScopeKindFunction s_kind)
    | Expr (ExprAnonymousFun {args;
                              body;
                              call}) -> Ok (ScopeKindLambda s_kind)
    | Expr (ExprModule {id;
                        visibility;
                        body}) -> Ok (ScopeKindModule s_kind)
    | Expr (ExprClass {id;
                       body}) -> Ok (ScopeKindClass s_kind)
    | _ -> Error (ErrorIdUnexpectedScope)

module FindInScope = struct
    let find_in_function s_kind = 
        ()
    
    let find_in_lambda s_kind = 
        ()

    let find_in_module s_kind = 
        ()

    let find_in_class s_kind = 
        ()

    let find_in_global s_kind = 
        ()
end
