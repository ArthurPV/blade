open Ast

type stream_ast = {
    mutable kind: ast_kind CCVector.vector;
}

val new_stream_ast : stream_ast

val push_ast : stream_ast -> ast_kind -> unit
