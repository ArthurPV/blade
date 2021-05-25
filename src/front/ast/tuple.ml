open Ast
open LilyFront.Error

let valid_tuple_expr kind = 
    match kind with
    | Expr (ExprBinop (l,b,r)) -> Ok (ExprBinop (l,b,r))
    | Expr (ExprUnary (l,u)) -> Ok (ExprUnary (l,u))
    | Expr (ExprFunCall (n,a)) -> Ok (ExprFunCall (n,a))
    | Expr (ExprVarCall (n)) -> Ok (ExprVarCall (n))
    | Expr (ExprConstCall (n)) -> Ok (ExprConstCall (n))
    | Expr (ExprAnonymousFun (e,a,c)) -> Ok (ExprAnonymousFun (e,a,c))
    | _ -> Error (ErrorIdUnexpectedExpr)
