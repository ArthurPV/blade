module Lf = LilyFront

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
    | LilyTypeArray of lily_type
    | LilyTypeTuple of lily_type array
    | LilyTypeUnit
    | LilyTypePolymorphic
    | LilyTypeAnonymousFuncton
    | LilyTypeNil
    | LilyTypeUndef
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
    | ExprBinop of {
        left: expr;
        binop: binop;
        right: expr;
    } 
    | ExprUnary of {
        unary: unary;
        right: expr;
    }
    | ExprVarDefine of expr (* var a *)
    | ExprVarDeclareTypeAndAssign of {
        id: expr;
        tp: lily_type;
        expr: expr;
    } (* var a :: <type> = <expr> *)
    | ExprVarDefineType of {
        id: expr;
        tp: lily_type;
    } (* var a :: <type> *)
    | ExprVarAssign of {
        id: expr;
        expr: expr;
    } (* var a = <expr> *)
    | ExprConstDefine of expr (* const a *)
    | ExprConstDeclareTypeAndAssign of {
        id: expr;
        tp: lily_type;
        expr: expr;
    } (* const a :: <type> = <expr> *)
    | ExprConstDefineType of {
        id: expr;
        tp: lily_type;
    } (* const a :: <type> *)
    | ExprConstAssign of {
        id: expr;
        expr: expr;
    } (* const a = <expr> *)
    | ExprVariableCall of expr (* a *)
    | ExprFunDefine of {
        id: expr;
        tp: lily_type array;
        ret: lily_type;
    } (* sum :: <type> -> <type> -> <return value> *)
    | ExprFunDeclare of {
        id: expr;
        visibility: visibility;
        args: lily_type;
        body: expr array;
    } (* fun sum <id> <id> = <body> *)
    | ExprFunCall of {
        id: expr;
        args: expr array;
    } (* sum(3, 4) *)
    | ExprAnonymousFun of {
        args: expr array;
        body: expr array;
        call: expr array;
    } (* (lambda x y -> x + y end)3 2 *)
    | ExprArray of {
        tp: lily_type;
        items: expr array;
    } (* [2,1,2] *)
    | ExprTuple of {
        tp: lily_type;
        items: expr array;
    } (* (1,2) *)
    | ExprIdentifier of string
    | ExprImport of literal (* import <module> *)
    | ExprShare of expr array (* share <module name>,<module name>,... *)
    | ExprAwait of expr (* await <expr> *)
    | ExprTypeDefine of expr (* type T *)
    | ExprTypeDeclare of {
        id: expr;
        field: (expr * lily_type) array;
    } (* type Person = name:string, age:int end*)
    | ExprTypeAssignField of {
        id: expr;
        expr: expr;
    } (* n = 10 *)
    | ExprTypeCall of {
        id: expr;
        field: (expr * expr) array;
    } (* <expr>{<field>,...} *)
    | ExprDataDefine of {
        id: expr;
        args: (lily_type option) array; (* data T<#a> *)
    } (* data T *)
    | ExprDataDeclare of {
        id: expr;
        args: (lily_type option) array; (* data T<#a> *)
        visibility: visibility;
        field: expr array;
        constructor: (lily_type option) array;
    } (* data Person = Name of string, Age of u16 end *)
    | ExprExplicitModule of {
        id: expr;
        body: expr array;
    } (* explicit module <expr> = <body> *)
    | ExprModule of {
        id: expr;
        visibility: visibility;
        body: expr array;
    } (* module <expr> = <body> *)
    | ExprInitClass of {
        args: expr array;
        body: expr array;
    } (* init name age = 
             var name = name
             var age = age
         end 
      *)
    | ExprExplicitClass of {
        id: expr;
        body: expr array;
    } (* explicit class <expr> = <body> *)
    | ExprClass of {
        id: expr;
        body: expr array;
    } (* class <expr> = <body> *)
    | ExprCallClass of {
        id: expr;
        args: expr array;
    } (* new <expr>(<arg>,<arg>) *)
    | ExprMacroDeclare of {
        id: expr;
        args: expr array;
        body: expr array;
    } (* macro <expr> <arg>,... = <body> *)
    | ExprMacroCall of {
        id: expr;
        args: expr array;
    } (* @<expr>(<arg>,...) *)
    | ExprNewline
    | ExprCommentOneLine
    | ExprCommentMultiLine
    | ExprCommentDoc of string
    | ExprLiteral of literal

type stmt =
    | StmtIf of {
        cond: expr;
        body: expr array;
    }
    | StmtSwitch of {
        elem: expr;
        body: expr array;
    }
    | StmtCase of {
        cond: expr;
        body: expr array;
    }
    | StmtBreak
    | StmtWhile of {
        cond: expr array;
        body: expr array;
    }
    | StmtFor of {
        cond: expr;
        body: expr array;
    }
    | StmtLoop of expr array
    | StmtReturn of expr

type ast_kind = 
    | Expr of expr
    | Stmt of stmt

type ast = { 
    stream: Lf.Stream.stream_token;
    filename: string;
    mutable current_token: Lf.Token.token;
    mutable current_location: Lf.Stream.location;
    mutable pos: int;
}

let new_ast st lex = {
    stream = st;
    filename = lex.Lf.Lexer.read.filename;
    current_token = Stdlib.Array.get st.tok 0;
    current_location = Stdlib.Array.get st.loc 0;
    pos = 0;
}

let ast_kind_to_str kind =
    match kind with
    | Expr (ExprBinop {left = _;
                       binop = BinopAdd;
                       right = _}) -> "Addition"
    | Expr (ExprBinop {left = _;
                       binop = BinopSub;
                       right = _}) -> "Substract"
    | Expr (ExprBinop {left = _;
                       binop = BinopMul;
                       right = _}) -> "Multiplication"
    | Expr (ExprBinop {left = _;
                       binop = BinopDiv;
                       right = _}) -> "Division"
    | Expr (ExprBinop {left = _;
                       binop = BinopMod;
                       right = _}) -> "Modulo"
    | Expr (ExprBinop {left = _;
                       binop = BinopPow;
                       right = _}) -> "Pow"
    | Expr (ExprBinop {left = _;
                       binop = BinopMerge;
                       right = _}) -> "Merge"
    | Expr (ExprBinop {left = _;
                       binop = BinopReplace;
                       right = _}) -> "Replace"
    | Expr (ExprBinop {left = _;
                       binop = BinopAddAssign;
                       right = _}) -> "Addition Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopSubAssign;
                       right = _}) -> "Substract Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopMulAssign;
                       right = _}) -> "Multiplication Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopDivAssign;
                       right = _}) -> "Division Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopModAssign;
                       right = _}) -> "Modulo Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopPowAssign;
                       right = _}) -> "Pow Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopAssign;
                       right = _}) -> "Assign"
    | Expr (ExprBinop {left = _;
                       binop = BinopEq;
                       right = _}) -> "Equal"
    | Expr (ExprBinop {left = _;
                       binop = BinopNotEq;
                       right = _}) -> "NotEqual"
    | Expr (ExprBinop {left = _;
                       binop = BinopIntervalEq;
                       right = _}) -> "IntervalEqual"
    | Expr (ExprBinop {left = _;
                       binop = BinopInterval;
                       right = _}) -> "Interval"
    | Expr (ExprBinop {left = _;
                       binop = BinopEqInterval;
                       right = _}) -> "EqInterval"
    | Expr (ExprBinop {left = _;
                       binop = BinopLess;
                       right = _}) -> "Less"
    | Expr (ExprBinop {left = _;
                       binop = BinopLessEq;
                       right = _}) -> "LessEqual"
    | Expr (ExprBinop {left = _;
                       binop = BinopGreater;
                       right = _}) -> "Greater"
    | Expr (ExprBinop {left = _;
                       binop = BinopGreaterEq;
                       right = _}) -> "GreaterEqual"
    | Expr (ExprBinop {left = _;
                       binop = BinopCondition;
                       right = _}) -> "BinopCondition"
    | Expr (ExprBinop {left = _;
                       binop = BinopAnd;
                       right = _}) -> "And"
    | Expr (ExprBinop {left = _;
                       binop = BinopOr;
                       right = _}) -> "Or"
    | Expr (ExprUnary {unary = UnaryPositive;
                       right = _}) -> "Positive"
    | Expr (ExprUnary {unary = UnaryNegative;
                       right = _}) -> "Negative"
    | Expr (ExprUnary {unary = UnaryNot;
                       right = _}) -> "Not"
    | Expr (ExprVarDefine _) -> "VarDefine"
    | Expr (ExprVarDeclareTypeAndAssign {id = _;
                                         tp = _;
                                         expr = _}) -> "VarDeclareTypeAndAssign"
    | Expr (ExprVarDefineType {id = _;
                               tp = _}) -> "VarDefineType"
    | Expr (ExprVarAssign {id = _;
                           expr = _}) -> "VarAssign"
    | Expr (ExprConstDefine _) -> "ConstDefine"
    | Expr (ExprConstDeclareTypeAndAssign {id = _;
                                           tp = _;
                                           expr = _}) -> "ConstDeclareTypeAndAssign"
    | Expr (ExprConstDefineType {id = _;
                                 tp = _}) -> "ConstDefineType"
    | Expr (ExprConstAssign {id = _;
                             expr = _}) -> "ConstAssign"
    | Expr (ExprVariableCall _) -> "VarCall"
    | Expr (ExprFunDefine {id = _;
                           tp = _;
                           ret = _}) -> "FunDefine"
    | Expr (ExprFunDeclare {id = _;
                            visibility = _;
                            args = _;
                            body = _}) -> "FunDeclare"
    | Expr (ExprFunCall {id = _;
                         args = _}) -> "FunCall"
    | Expr (ExprAnonymousFun {args = _;
                              body = _;
                              call = _}) -> "AnonymousFun"
    | Expr (ExprArray {tp = _;
                       items = _}) -> "Array"
    | Expr (ExprTuple {tp = _;
                       items = _}) -> "Tuple"
    | Expr (ExprImport (_)) -> "Import"
    | Expr (ExprShare (_)) -> "Share"
    | Expr (ExprAwait (_)) -> "Await"
    | Expr (ExprTypeDefine (_)) -> "TypeDefine"
    | Expr (ExprTypeDeclare {id = _;
                             field = _}) -> "TypeDeclare"
    | Expr (ExprTypeAssignField {id = _;
                                 expr = _}) -> "Field"
    | Expr (ExprTypeCall {id = _;
                          field = _}) -> "TypeCall"
    | Expr (ExprDataDefine {id = _;
                            args = _}) -> "DataDefine"
    | Expr (ExprDataDeclare {id = _;
                             args = _;
                             visibility = _;
                             field = _;
                             constructor = _}) -> "DataDeclare"
    | Expr (ExprExplicitModule {id = _;
                                body = _}) -> "ExplicitModule"
    | Expr (ExprModule {id = _;
                        visibility = _;
                        body = _}) -> "Module"
    | Expr (ExprInitClass {args = _;
                           body = _}) -> "InitClass"
    | Expr (ExprExplicitClass {id = _;
                               body = _}) -> "ExplicitClass"
    | Expr (ExprClass {id = _;
                       body = _}) -> "Class"
    | Expr (ExprCallClass {id = _;
                           args = _}) -> "CallClass"
    | Expr (ExprMacroDeclare {id = _;
                              args = _;
                              body = _}) -> "MacroDeclare"
    | Expr (ExprMacroCall {id = _;
                           args = _}) -> "MacroCall"
    | Expr (ExprNewline) -> "Newline"
    | Expr (ExprCommentOneLine) -> "CommentOneLine"
    | Expr (ExprCommentMultiLine) -> "CommentMultiLine"
    | Expr (ExprCommentDoc s) -> Printf.sprintf "CommentDoc -> %s" s
    | _ -> "Unknwon"
