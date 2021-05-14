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

type 'a value = 
    | Literal of 'a literal
    | Array

type lily_type = 
    | LilyTypeBool
    | LilyTypeString
    | LilyTypeChar
    | LilyTypeI8
    | LilyTypeI16
    | LilyTypeI32
    | LilyTypeI64
    | LilyTypeI128
    | LilyTypeU8
    | LilyTypeU16
    | LilyTypeU32
    | LilyTypeU64
    | LilyTypeU128
    | LilyTypeArray
    | LilyTypeUnit
    | LilyTypeGeneric
    | LilyTypeUserDefinedType of string

type 'a expr = 
    | ExprBinop of 'a expr * 'a binop * 'a expr
    | ExprUnary of 'a expr * 'a unary
    | ExprVarDefine of 'a expr
    | ExprVarDeclareType of 'a expr * lily_type * 'a value
    | ExprVarDeclare of 'a expr * 'a value
    | ExprVarCall of 'a expr
    | ExprConstDefine of 'a expr
    | ExprConstDeclareType of 'a expr * lily_type * 'a value
    | ExprConstDeclare of 'a expr * 'a value
    | ExprConstCall of 'a expr
    | ExprFunDefine of 'a expr * (lily_type CCVector.vector) * 'a value
    | ExprFunDeclare
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
