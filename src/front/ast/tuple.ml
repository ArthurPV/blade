module Lf = LilyFront

let valid_tuple_expr kind = 
    match kind with
    | Ast.Expr (ExprBinop {left;
                           binop;
                           right}) -> Ok (Ast.ExprBinop {left = left;
                                                         binop = binop; 
                                                         right = right})
    | Ast.Expr (ExprUnary {unary;
                           right}) -> Ok (Ast.ExprUnary {unary = unary;
                                                         right = right})
    | Ast.Expr (ExprFunCall {id;
                             args}) -> Ok (Ast.ExprFunCall {id = id;
                                                            args = args})
    (*| Expr (ExprAnonymousFun (e,a,c)) -> Ok (ExprAnonymousFun (e,a,c))*)
    | _ -> Error (Lf.Error.ErrorIdUnexpectedExpr)
