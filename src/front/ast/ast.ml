open Kwhale_front.Stream
open Kwhale_front.Token

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

type 'a expr = 
    | ExprBinop of 'a binop
    | ExprUnary of 'a unary
    | ExprFunCall
    | ExprIdentifier of string
    | ExprLiteralBool of bool
    | ExprLiteralInt of int
    | ExprLiteralFloat of float
    | ExprLiteralString of string
    | ExprLiteralChar of char

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

let new_ast st = {
    stream = st;
    current_token = CCVector.get st.tok 0;
    current_location = CCVector.get st.loc 0;
    pos = 0;
}
