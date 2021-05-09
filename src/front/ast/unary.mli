open Ast
open LilyFront.Error
open LilyFront.Token

val token_to_unary : 'a token -> ('a unary, error_id) result
