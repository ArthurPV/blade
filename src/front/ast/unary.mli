open Ast
open LilyFront.Error
open LilyFront.Token

val token_to_unary : token -> (unary, error_id) result
