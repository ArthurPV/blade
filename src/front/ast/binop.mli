open Ast
open LilyFront.Error
open LilyFront.Token

val token_to_binop : token -> (binop, error_id) result
