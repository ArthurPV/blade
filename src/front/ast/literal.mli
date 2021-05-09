open LilyFront.Error
open LilyFront.Token

val token_to_literal : 'a token -> ('a Ast.literal, error_id) result
