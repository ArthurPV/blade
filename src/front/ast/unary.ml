open Ast
open LilyFront.Error
open LilyFront.Token

let token_to_unary tok = 
    match tok with
    | Operator OperatorMinus -> Ok UnaryNegative
    | Operator OperatorPlus -> Ok UnaryPositive
    | Keyword KeywordNot -> Ok UnaryNot
    | _ -> Error ErrorIdInvalidUnary
