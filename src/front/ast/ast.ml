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
    | ExprVarDefine of expr (* var a *)
    | ExprVarDeclareTypeAndAssign of expr * lily_type * expr (* var a :: <type> = <expr> *)
    | ExprVarDefineType of expr * lily_type (* var a :: <type> *)
    | ExprVarAssign of expr * expr (* var a = <expr> *)
    | ExprVarCall of expr (* a *)
    | ExprConstDefine of expr (* const a *)
    | ExprConstDeclareTypeAndAssign of expr * lily_type * expr (* const a :: <type> = <expr> *)
    | ExprConstDefineType of expr * lily_type (* const a :: <type> *)
    | ExprConstAssign of expr * expr (* const a = <expr> *)
    | ExprConstCall of expr (* a *)
    | ExprFunDefine of expr * (lily_type CCVector.vector) * value (* sum :: <type> -> <type> -> <return value> *)
    | ExprFunDeclare of expr * (lily_type CCVector.vector) * (expr CCVector.vector) (* fun sum <id> <id> =  *)
    | ExprFunCall of expr * (expr CCVector.vector) (* sum(3, 4) *)
    | ExprIdentifier of string
    | ExprNewline
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

let new_ast st lex = {
    stream = st;
    filename = lex.read.filename;
    current_token = CCVector.get st.tok 0;
    current_location = CCVector.get st.loc 0;
    pos = 0;
}

let ast_kind_to_str kind =
    match kind with
    | Expr (ExprBinop (_,BinopAdd,_)) -> "Addition"
    | Expr (ExprBinop (_,BinopSub,_)) -> "Substract"
    | Expr (ExprBinop (_,BinopMul,_)) -> "Multiplication"
    | Expr (ExprBinop (_,BinopDiv,_)) -> "Division"
    | Expr (ExprBinop (_,BinopMod,_)) -> "Modulo"
    | Expr (ExprBinop (_,BinopPow,_)) -> "Pow"
    | Expr (ExprBinop (_,BinopMerge,_)) -> "Merge"
    | Expr (ExprBinop (_,BinopReplace,_)) -> "Replace"
    | Expr (ExprBinop (_,BinopAddAssign,_)) -> "Addition Assign"
    | Expr (ExprBinop (_,BinopSubAssign,_)) -> "Substract Assign"
    | Expr (ExprBinop (_,BinopMulAssign,_)) -> "Multiplication Assign"
    | Expr (ExprBinop (_,BinopDivAssign,_)) -> "Division Assign"
    | Expr (ExprBinop (_,BinopModAssign,_)) -> "Modulo Assign"
    | Expr (ExprBinop (_,BinopPowAssign,_)) -> "Pow Assign"
    | Expr (ExprBinop (_,BinopAssign,_)) -> "Assign"
    | Expr (ExprBinop (_,BinopEq,_)) -> "Equal"
    | Expr (ExprBinop (_,BinopNotEq,_)) -> "Not Equal"
    | Expr (ExprBinop (_,BinopIntervalEq,_)) -> "Interval Equal"
    | Expr (ExprBinop (_,BinopInterval,_)) -> "Interval"
    | Expr (ExprBinop (_,BinopEqInterval,_)) -> "Eq Interval"
    | Expr (ExprBinop (_,BinopLess,_)) -> "Less"
    | Expr (ExprBinop (_,BinopLessEq,_)) -> "Less Equal"
    | Expr (ExprBinop (_,BinopGreater,_)) -> "Greater"
    | Expr (ExprBinop (_,BinopGreaterEq,_)) -> "Greater Equal"
    | Expr (ExprBinop (_,BinopCondition,_)) -> "Condition Unary"
    | Expr (ExprBinop (_,BinopAnd,_)) -> "And"
    | Expr (ExprBinop (_,BinopOr,_)) -> "Or"
    | Expr (ExprUnary (_,UnaryPositive)) -> "Positive"
    | Expr (ExprUnary (_,UnaryNegative)) -> "Negative"
    | Expr (ExprUnary (_,UnaryNot)) -> "Not"
    | Expr (ExprVarDefine _) -> "Var Define"
    | Expr (ExprVarDeclareTypeAndAssign (_,_,_)) -> "Var Declare Type and Assign"
    | Expr (ExprVarDefineType (_,_)) -> "Var Define Type"
    | Expr (ExprVarAssign (_,_)) -> "Var Assign"
    | Expr (ExprVarCall _) -> "Var Call"
    | Expr (ExprConstDefine _) -> "Const Define"
    | Expr (ExprConstDeclareTypeAndAssign (_,_,_)) -> "Const Declare Type and Assign"
    | Expr (ExprConstDefineType (_,_)) -> "Const Define Type"
    | Expr (ExprConstAssign (_,_)) -> "Const Assign"
    | Expr (ExprFunDefine (_,_,_)) -> "Fun Define"
    | Expr (ExprFunDeclare (_,_,_)) -> "Fun Declare"
    | Expr (ExprFunCall (_,_)) -> "Fun Call"
    | _ -> "Unknwon"
