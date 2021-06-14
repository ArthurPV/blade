open LilyAst.Ast

type check_name = {
    ast: ast;
    mutable declaration: ast_kind CCVector.vector;
    mutable current_declaration: ast_kind;
}

let get_check_declaration ast = 
    let v = CCVector.create () in
    v

let new_check_name ast decl = {
    ast = ast;
    declaration = decl;
    current_declaration = CCVector.get decl 0;
}
