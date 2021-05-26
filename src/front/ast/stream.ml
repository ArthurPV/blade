open Ast

type stream_ast = {
    mutable kind: ast_kind array
}

let new_stream_ast = {
    kind = [||];
}

let push_ast sa kind = 
    sa.kind <- Stdlib.Array.append sa.kind [|kind|]
