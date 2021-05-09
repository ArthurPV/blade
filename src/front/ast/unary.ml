open Ast
open Kwhale_front.Error
open Kwhale_front.Token

let token_to_unary tok = 
    match tok with
    | Operator OperatorMinus -> Ok UnaryNegative
    | Operator OperatorPlus -> Ok UnaryPositive
    | Keyword KeywordNot -> Ok UnaryNot
    | _ -> Error ErrorIdInvalidUnary
