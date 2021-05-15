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
    | ExprVarDefine of expr (* var a *)
    | ExprVarDeclareType of expr * lily_type * value (* var a :: <type> = <value> *)
    | ExprVarDeclare of expr * value (* var a = <value> *)
    | ExprVarCall of expr (* a *)
    | ExprConstDefine of expr (* const a *)
    | ExprConstDeclareType of expr * lily_type * value (* const a :: <type> = <value> *)
    | ExprConstDeclare of expr * value (* const a = <value> *)
    | ExprConstCall of expr (* a *)
    | ExprFunDefine of expr * (lily_type CCVector.vector) * value (* sum :: <type> -> <type> -> <return value> *)
    | ExprFunDeclare (* fun sum <id> <id> =  *)
    | ExprFunCall (* sum(3, 4) *)
    | ExprIdentifier of string
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
    mutable current_token: token;
    mutable current_location: location;
    mutable pos: int;
}

let new_ast st = {
    stream = st;
    current_token = CCVector.get st.tok 0;
    current_location = CCVector.get st.loc 0;
    pos = 0;
}

let ast_kind_to_str kind =
    match kind with
    | Expr (ExprBinop (_,BinopAdd,_)) -> "add"
    | Expr (ExprBinop (_,BinopSub,_)) -> "sub"
    | Expr (ExprBinop (_,BinopMul,_)) -> "mul"
    | Expr (ExprBinop (_,BinopDiv,_)) -> "div"
    | Expr (ExprBinop (_,BinopMod,_)) -> "div"
    | Expr (ExprBinop (_,BinopPow,_)) -> "pow"
    | Expr (ExprBinop (_,BinopMerge,_)) -> "merge"
    | Expr (ExprBinop (_,BinopReplace,_)) -> "replace"
    | Expr (ExprBinop (_,BinopAddAssign,_)) -> "add assign"
    | Expr (ExprBinop (_,BinopSubAssign,_)) -> "sub assign"
    | Expr (ExprBinop (_,BinopMulAssign,_)) -> "mul assign"
    | Expr (ExprBinop (_,BinopDivAssign,_)) -> "div assign"
    | Expr (ExprBinop (_,BinopModAssign,_)) -> "mod assign"
    | Expr (ExprBinop (_,BinopPowAssign,_)) -> "pow assign"
    | Expr (ExprBinop (_,BinopAssign,_)) -> "assign"
    | Expr (ExprBinop (_,BinopEq,_)) -> "eq"
    | Expr (ExprBinop (_,BinopNotEq,_)) -> "not eq"
    | Expr (ExprBinop (_,BinopIntervalEq,_)) -> "interval eq"
    | Expr (ExprBinop (_,BinopInterval,_)) -> "interval"
    | Expr (ExprBinop (_,BinopEqInterval,_)) -> "eq interval"
    | Expr (ExprBinop (_,BinopLess,_)) -> "less"
    | Expr (ExprBinop (_,BinopLessEq,_)) -> "less eq"
    | Expr (ExprBinop (_,BinopGreater,_)) -> "greater"
    | Expr (ExprBinop (_,BinopGreaterEq,_)) -> "greater eq"
    | Expr (ExprBinop (_,BinopCondition,_)) -> "condition"
    | Expr (ExprBinop (_,BinopAnd,_)) -> "and"
    | Expr (ExprBinop (_,BinopOr,_)) -> "or"
    | Expr (ExprUnary (_,UnaryPositive)) -> "positive"
    | Expr (ExprUnary (_,UnaryNegative)) -> "negative"
    | Expr (ExprUnary (_,UnaryNot)) -> "not"
    | Expr (ExprVarDefine _) -> "var define"
    | Expr (ExprVarDeclareType (_,_,_)) -> "var declare type"
    | Expr (ExprVarDeclare (_,_)) -> "var declare"
    | Expr (ExprVarCall _) -> "var call"
    | Expr (ExprConstDefine _) -> "const define"
    | Expr (ExprConstDeclareType (_,_,_)) -> "const declare type"
    | Expr (ExprConstDeclare (_,_)) -> "const declare"
    | Expr (ExprFunDefine (_,_,_)) -> "fun define"
    | Expr (ExprFunDeclare) -> "fun declare"
    | Expr (ExprFunCall) -> "fun call"
    | _ -> "unknwon"
