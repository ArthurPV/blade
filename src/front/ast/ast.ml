open Blade_front.Stream
open Blade_front.Token

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

type stmt =
    | StmtIf
    | StmtSwitch
    | StmtBreak
    | StmtWhile
    | StmtFor
    | StmtLoop

type 'a ast_kind = 
    | Expr of expr
    | Stmt of stmt

(*let iter_stream_token st = 
    Stack.iter st.tok*)

(*let iter_stream_location st = 
    Stack.iter st.loc*)

type 'a ast = { 
    stream: 'a stream_token;
    mutable current_token: 'a token;
    mutable current_location: 'a location;
    mutable pos: int;
}
