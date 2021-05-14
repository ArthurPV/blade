open Ast

type 'a stream_ast = {
    mutable kind: 'a ast_kind CCVector.vector
}

val new_stream_ast : 'a stream_ast

val push_ast : 'a stream_ast -> 'a ast_kind -> unit
