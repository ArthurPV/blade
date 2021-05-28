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
        left: expr;
        unary: unary;
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
    | ExprVariableReassign of {
        id: expr;
        expr: expr;
    } (* a = <expr> *)
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
    | ExprImport of lily_type (* import <module> *)
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

val new_ast : Lf.Stream.stream_token -> Lf.Lexer.lexer -> ast

val ast_kind_to_str : ast_kind -> string
