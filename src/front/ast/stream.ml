open Ast

type 'a stream_ast = {
    mutable kind: 'a ast_kind CCVector.vector
}

let new_stream_ast = {
    kind = CCVector.create ();
}

let push_ast sa kind = 
    CCVector.push sa.kind kind
