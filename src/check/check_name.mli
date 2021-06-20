open LilyAst.Ast
open LilyAst.Stream

type check_name = {
    ast: ast;
    stream: stream_ast;
    mutable declaration: ast_kind CCVector.vector;
    mutable current_declaration: ast_kind;
}

(*val get_check_declaration : ast_kind CCVector.vector -> ast_kind CCVector.vector 
val new_check_name : ast -> ast_kind CCVector.vector -> stream_ast -> check_name*)
