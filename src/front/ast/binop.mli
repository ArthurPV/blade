open Ast
open LilyFront.Error
open LilyFront.Token

val token_to_binop : 'a token -> ('a binop, error_id) result
