open Ast

type stream_ast = {
    mutable kind: ast_kind CCVector.vector
}

let new_stream_ast = {
    kind = CCVector.create ();
}

let push_ast sa kind = 
    CCVector.push sa.kind kind
