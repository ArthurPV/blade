open Ast
open LilyFront.Error

val token_to_literal : ast -> (literal, error_id) result
