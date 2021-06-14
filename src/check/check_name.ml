open LilyAst.Ast

type check_name = {
    mutable declaration: ast_kind CCVector.vector;
    mutable current_declaration: ast_kind;
}
