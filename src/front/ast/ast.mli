type binop = 
    | BinopAdd
    | BinopSub
    | BinopMul
    | BinopDiv
    | BinopMod
    | BinopPow
    | BinopMerge
    | BinopReplace
    | BinopAddAssign
    | BinopSubAssign
    | BinopMulAssign
    | BinopDivAssign
    | BinopModAssign
    | BinopPowAssign

type unary = 
    | UnaryPositive
    | UnaryNegative
    | UnaryNot

type expr = 
    | ExprBinop of binop
    | ExprUnary of unary
    | ExprLiteralBool of bool
    | ExprLiteralInt of int
    | ExprLiteralFloat of float
    | ExprLiteralString of string
    | ExprLiteralChar of char

type 'a ast = 
    | Expr of expr
