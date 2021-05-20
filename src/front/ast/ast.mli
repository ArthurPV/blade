open LilyFront.Lexer
open LilyFront.Stream
open LilyFront.Token

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

type unary = 
    | UnaryPositive
    | UnaryNegative
    | UnaryNot

type literal =
    | LiteralBool of bool
    | LiteralInt of int
    | LiteralFloat of float
    | LiteralString of string
    | LiteralChar of char

type value = 
    | Literal of literal
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
    | LilyTypePolymorphic
    | LilyTypeUserDefinedType of string

type expr = 
    | ExprBinop of expr * binop * expr
    | ExprUnary of expr * unary
    | ExprVarDefine of expr
    | ExprVarDeclareTypeAndAssign of expr * lily_type * expr
    | ExprVarDefineType of expr * lily_type
    | ExprVarAssign of expr * expr
    | ExprVarCall of expr
    | ExprConstDefine of expr
    | ExprConstDeclareTypeAndAssign of expr * lily_type * expr
    | ExprConstDefineType of expr * lily_type
    | ExprConstAssign of expr * expr
    | ExprConstCall of expr
    | ExprVariableReassign of expr * expr
    | ExprFunDefine of expr * (lily_type CCVector.vector) * value
    | ExprFunDeclare of expr * (lily_type CCVector.vector) * (expr CCVector.vector)
    | ExprFunCall of expr * (expr CCVector.vector)
    | ExprIdentifier of string
    | ExprNewline
    | ExprCommentOneLine
    | ExprCommentMultiLine
    | ExprCommentDoc of string
    | ExprLiteral of literal

type stmt =
    | StmtIf
    | StmtSwitch
    | StmtBreak
    | StmtWhile
    | StmtFor
    | StmtLoop

type ast_kind = 
    | Expr of expr
    | Stmt of stmt

type ast = { 
    stream: stream_token;
    filename: string;
    mutable current_token: token;
    mutable current_location: location;
    mutable pos: int;
}

val new_ast : stream_token -> lexer -> ast

val ast_kind_to_str : ast_kind -> string
