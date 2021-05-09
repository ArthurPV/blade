open Ast
open Kwhale_front.Error
open Kwhale_front.Token

val token_to_binop : 'a token -> ('a binop, error_id) result
