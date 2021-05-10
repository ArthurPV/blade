open LilyFront.Stream
open LilyFront.Token

type 'a binop = 
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
    | BinopNotEq
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

type 'a unary = 
    | UnaryPositive
    | UnaryNegative
    | UnaryNot

type 'a literal =
    | LiteralBool of bool
    | LiteralInt of int
    | LiteralFloat of float
    | LiteralString of string
    | LiteralChar of char

type 'a expr = 
    | ExprBinop of 'a expr * 'a binop * 'a expr
    | ExprUnary of 'a expr * 'a unary
    | ExprFunCall
    | ExprIdentifier of string
    | ExprLiteral of 'a literal

type 'a stmt =
    | StmtIf
    | StmtSwitch
    | StmtBreak
    | StmtWhile
    | StmtFor
    | StmtLoop

type 'a ast_kind = 
    | Expr of 'a expr
    | Stmt of 'a stmt

type 'a ast = { 
    stream: 'a stream_token;
    mutable current_token: 'a token;
    mutable current_location: 'a location;
    mutable pos: int;
}

val new_ast : 'a stream_token -> 'a ast

val ast_kind_to_str : 'a ast_kind -> string
