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

type visibility = 
    | Private
    | Public

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

(*
All expressions:

-------------------------------------------------
Comments:
** comment one line
(* comment multi line *)
(**  comment doc **)
-------------------------------------------------

-------------------------------------------------
Variables:
Var:
var a 
var a = 5
var a :: i32 = 5
var a :: i32

Const:
const a
const a = 5
const a :: i32 = 5
const a :: i32
-------------------------------------------------

-------------------------------------------------
Binop:
Arithmetics:
3 + 3
3 - 3
3 * 3
3 / 3
3 % 3
3 ^ 3
Assigns:
a += 3
a -= 3
a *= 3
a /= 3
a %= 3
a ^= 3
Comparaisons:
3 == 3
3 <> 3
3 < 2
3 > 2
3 <= 2
3 >= 3
Logicals:
3 < 2 and 3 > 2
3 < 2 or 3 > 2
Merge and Replace:
[3,2,3] ++ [2,3,3]
[3,2,2] -- [1,2,3]
-------------------------------------------------

-------------------------------------------------
Functions:
add :: i32 -> i32 -> i32
fun add x y = 3 + 3
pub fun add x y = 3 + 3
pub async fun add x y = 3 + 3
(x y -> x + y end)3 4
-------------------------------------------------

-------------------------------------------------
Modules:
explicit module Calculator = 
    add :: i32 -> i32 -> i32
    sub :: i32 -> i32 -> i32
end

module Calculator = 
    fun add x y = x + y end
    fun sub x y = x - y end
end
-------------------------------------------------

-------------------------------------------------
Others:
await add(3,2)
-------------------------------------------------
*)

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
    | ExprVariableReassign of expr * expr (* a = <expr> *)
    | ExprFunDefine of expr * (lily_type array) * lily_type (* sum :: <type> -> <type> -> <return value> *)
    | ExprFunDeclare of expr * (lily_type array) * (expr array) (* fun sum <id> <id> =  *)
    | ExprFunCall of expr * (expr array) (* sum(3, 4) *)
    | ExprAnonymousFun of (expr array) * (expr array) * (expr array) (* (lambda x y -> x + y end)3 2 *)
    | ExprArray of lily_type * (expr array)
    | ExprTuple of lily_type * (expr array)
    | ExprIdentifier of string
    | ExprImport of lily_type (* import <module> *)
    | ExprShare of expr (* share <module name> *)
    | ExprAwait of expr (* await <expr> *)
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
    | StmtReturn

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
    current_token = Stdlib.Array.get st.tok 0;
    current_location = Stdlib.Array.get st.loc 0;
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
    | Expr (ExprBinop (_,BinopNotEq,_)) -> "NotEqual"
    | Expr (ExprBinop (_,BinopIntervalEq,_)) -> "IntervalEqual"
    | Expr (ExprBinop (_,BinopInterval,_)) -> "Interval"
    | Expr (ExprBinop (_,BinopEqInterval,_)) -> "EqInterval"
    | Expr (ExprBinop (_,BinopLess,_)) -> "Less"
    | Expr (ExprBinop (_,BinopLessEq,_)) -> "LessEqual"
    | Expr (ExprBinop (_,BinopGreater,_)) -> "Greater"
    | Expr (ExprBinop (_,BinopGreaterEq,_)) -> "GreaterEqual"
    | Expr (ExprBinop (_,BinopCondition,_)) -> "ConditionUnary"
    | Expr (ExprBinop (_,BinopAnd,_)) -> "And"
    | Expr (ExprBinop (_,BinopOr,_)) -> "Or"
    | Expr (ExprUnary (_,UnaryPositive)) -> "Positive"
    | Expr (ExprUnary (_,UnaryNegative)) -> "Negative"
    | Expr (ExprUnary (_,UnaryNot)) -> "Not"
    | Expr (ExprVarDefine _) -> "VarDefine"
    | Expr (ExprVarDeclareTypeAndAssign (_,_,_)) -> "VarDeclareTypeAndAssign"
    | Expr (ExprVarDefineType (_,_)) -> "VarDefineType"
    | Expr (ExprVarAssign (_,_)) -> "VarAssign"
    | Expr (ExprVarCall _) -> "VarCall"
    | Expr (ExprConstDefine _) -> "ConstDefine"
    | Expr (ExprConstDeclareTypeAndAssign (_,_,_)) -> "ConstDeclareTypeAndAssign"
    | Expr (ExprConstDefineType (_,_)) -> "ConstDefineType"
    | Expr (ExprConstAssign (_,_)) -> "ConstAssign"
    | Expr (ExprConstCall _) -> "ConstCall"
    | Expr (ExprVariableReassign (_,_)) -> "ReassignVariable"
    | Expr (ExprFunDefine (_,_,_)) -> "FunDefine"
    | Expr (ExprFunDeclare (_,_,_)) -> "FunDeclare"
    | Expr (ExprFunCall (_,_)) -> "FunCall"
    | Expr (ExprNewline) -> "Newline"
    | Expr (ExprCommentOneLine) -> "CommentOneLine"
    | Expr (ExprCommentMultiLine) -> "CommentMultiLine"
    | Expr (ExprCommentDoc s) -> Printf.sprintf "CommentDoc -> %s" s
    | _ -> "Unknwon"
