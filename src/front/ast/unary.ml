open Ast
open Blade_front.Error
open Blade_front.Token

let token_to_unary tok = 
    match tok with
    | Operator OperatorMinus -> Ok UnaryNegative
    | Operator OperatorPlus -> Ok UnaryPositive
    | Keyword KeywordNot -> Ok UnaryNot
    | _ -> Error ErrorIdInvalidUnary
