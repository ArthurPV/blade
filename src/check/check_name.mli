open LilyAst.Ast

type check_name = {
    ast: ast;
    mutable declaration: ast_kind CCVector.vector;
    mutable current_declaration: ast_kind;
}

val get_check_declaration : ast -> ast_kind CCVector.vector 
val new_check_name : ast -> ast_kind CCVector.vector -> check_name
