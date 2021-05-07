open Blade_front.Stream

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
    | BinopAssign
    | BinopEq
    | BinopIntervalEq
    | BinopInterval
    | BinopEqInterval
    | BinopLess
    | BinopLessEq
    | BinopGreater
    | BinopGreaterEq
    | BinopCondition
    | BinopAnd
    | BinopOr

type unary = 
    | UnaryPositive
    | UnaryNegative
    | UnaryNot

type expr = 
    | ExprBinop of binop
    | ExprUnary of unary
    | ExprIdentifier of string
    | ExprLiteralBool of bool
    | ExprLiteralInt of int
    | ExprLiteralFloat of float
    | ExprLiteralString of string
    | ExprLiteralChar of char

type 'a ast_kind = 
    | Expr of expr

type 'a ast = { 
    stream: 'a stream_token;
    pos: int;
}
