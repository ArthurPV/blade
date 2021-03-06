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
    | LiteralInt of string
    | LiteralFloat of string
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
    | LilyTypeTuple of lily_type CCVector.vector
    | LilyTypeUnit
    | LilyTypePolymorphic
    | LilyTypeAnonymousFuncton
    | LilyTypeNil
    | LilyTypeUndef
    | LilyTypeUserDefinedType of string

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
    | ExprVarDefine of expr (* var <id> *)
    | ExprVarDeclareTypeAndAssign of {
        id: expr;
        tp: lily_type;
        expr: expr;
    } (* var <id> :: <tp> = <expr> *)
    | ExprVarDefineType of {
        id: expr;
        tp: lily_type;
    } (* var <id> :: <tp> *)
    | ExprVarAssign of {
        id: expr;
        expr: expr;
    } (* var <id> = <expr> *)
    | ExprConstDefine of expr (* const <id> *)
    | ExprConstDeclareTypeAndAssign of {
        id: expr;
        tp: lily_type;
        expr: expr;
    } (* const <id> :: <tp> = <expr> *)
    | ExprConstDefineType of {
        id: expr;
        tp: lily_type;
    } (* const <id> :: <tp> *)
    | ExprConstAssign of {
        id: expr;
        expr: expr;
    } (* const <id> = <expr> *)
    | ExprVariableCall of expr (* a *)
    | ExprFunDefine of {
        id: expr;
        tp: lily_type CCVector.vector;
        ret: lily_type;
    } (* sum :: <type> -> <type> -> <return value> *)
    | ExprFunDeclare of {
        id: expr;
        visibility: visibility;
        args: lily_type;
        body: expr CCVector.vector;
    } (* fun sum <id> <id> = <body> *)
    | ExprFunCall of {
        id: expr;
        args: expr CCVector.vector;
    } (* sum(3, 4) *)
    | ExprAnonymousFun of {
        args: expr CCVector.vector;
        body: expr CCVector.vector;
        call: expr CCVector.vector;
    } (* (lambda x y -> x + y end)3 2 *)
    | ExprArray of {
        items: expr CCVector.vector;
    } (* [2,1,2] *)
    | ExprTuple of {
        items: expr CCVector.vector;
    } (* (1,2) *)
    | ExprIdentifier of string
    | ExprImport of literal (* import <module> *)
    | ExprShare of expr CCVector.vector (* share <module name>,<module name>,... *)
    | ExprAwait of expr (* await <expr> *)
    | ExprTypeDefine of expr (* type T *)
    | ExprTypeDeclare of {
        id: expr;
        field: (expr * lily_type) CCVector.vector;
    } (* type Person = name:string, age:int end*)
    | ExprTypeAssignField of {
        id: expr;
        expr: expr;
    } (* n = 10 *)
    | ExprTypeCall of {
        id: expr;
        field: (expr * expr) CCVector.vector;
    } (* <expr>{<field>,...} *)
    | ExprDataDefine of {
        id: expr;
        args: (lily_type option) CCVector.vector; (* data T<#a> *)
    } (* data T *)
    | ExprDataDeclare of {
        id: expr;
        args: (lily_type option) CCVector.vector; (* data T<#a> *)
        visibility: visibility;
        field: expr CCVector.vector;
        constructor: (lily_type option) CCVector.vector;
    } (* data Person = Name of string, Age of u16 end *)
    | ExprExplicitModule of {
        id: expr;
        body: expr CCVector.vector;
    } (* explicit module <expr> = <body> *)
    | ExprModule of {
        id: expr;
        visibility: visibility;
        body: expr CCVector.vector;
    } (* module <expr> = <body> *)
    | ExprInitClass of {
        args: expr CCVector.vector;
        body: expr CCVector.vector;
    } (* init name age = 
             var name = name
             var age = age
         end 
      *)
    | ExprExplicitClass of {
        id: expr;
        body: expr CCVector.vector;
    } (* explicit class <expr> = <body> *)
    | ExprClass of {
        id: expr;
        body: expr CCVector.vector;
    } (* class <expr> = <body> *)
    | ExprCallClass of {
        id: expr;
        args: expr CCVector.vector;
    } (* new <expr>(<arg>,<arg>) *)
    | ExprMacroDeclare of {
        id: expr;
        args: expr CCVector.vector;
        body: expr CCVector.vector;
    } (* macro <expr> <arg>,... = <body> *)
    | ExprMacroCall of {
        id: expr;
        args: expr CCVector.vector;
    } (* @<expr>(<arg>,...) *)
    | ExprCommentOneLine
    | ExprCommentMultiLine
    | ExprCommentDoc of string
    | ExprLiteral of literal

type stmt =
    | StmtIf of {
        cond: expr;
        body: expr CCVector.vector;
    } (* if <cond> then <body> end *)
    | StmtSwitch of {
        elem: expr;
        body: expr CCVector.vector;
    }
    | StmtCase of {
        cond: expr;
        body: expr CCVector.vector;
    }
    | StmtBreak
    | StmtWhile of {
        cond: expr CCVector.vector;
        body: expr CCVector.vector;
    }
    | StmtFor of {
        cond: expr;
        body: expr CCVector.vector;
    }
    | StmtLoop of expr CCVector.vector
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

val new_ast : Lf.Stream.stream_token -> Lf.Lexer.lexer -> ast

val ast_kind_to_str : ast_kind -> string
