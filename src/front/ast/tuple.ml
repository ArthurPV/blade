module Lf = LilyFront

let valid_tuple_expr kind = 
    match kind with
    | Ast.Expr (ExprBinop {left;
                           binop;
                           right}) -> Ok (Ast.ExprBinop {left = left;
                                                         binop = binop; 
                                                         right = right})
    | Ast.Expr (ExprUnary {left;
                           unary}) -> Ok (Ast.ExprUnary {left = left;
                                                         unary = unary})
    | Ast.Expr (ExprFunCall {id;
                             args}) -> Ok (Ast.ExprFunCall {id = id;
                                                            args = args})
    (*| Expr (ExprAnonymousFun (e,a,c)) -> Ok (ExprAnonymousFun (e,a,c))*)
    | _ -> Error (Lf.Error.ErrorIdUnexpectedExpr)
