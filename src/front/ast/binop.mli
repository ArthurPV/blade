open Ast
open Blade_front.Error
open Blade_front.Token

val token_to_binop : 'a token -> ('a binop, error_id) result
