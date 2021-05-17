open Ast
(*open Parser*)

let print_ast ast =
    Printf.printf "%d" (CCVector.length (ast.stream.tok))
    (*let rec loop ast = 
        if ast.pos < (CCVector.length (ast.stream.tok))-1 then
        match parser ast with
        | Error _ -> Printf.printf "error\n"; exit 1
        | Ok p (Printf.printf "%s" (ast_kind_to_str p);
                match p with
                | Expr (ExprBinop))*)
