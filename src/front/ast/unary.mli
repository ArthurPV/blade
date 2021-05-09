open Ast
open Kwhale_front.Error
open Kwhale_front.Token

val token_to_unary : 'a token -> ('a unary, error_id) result
