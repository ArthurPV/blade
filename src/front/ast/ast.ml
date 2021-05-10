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
    | _ -> "unknwon"
