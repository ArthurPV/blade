open Ast
open Blade_front.Error
open Blade_front.Token

val token_to_unary : 'a token -> (unary, error_id) result
