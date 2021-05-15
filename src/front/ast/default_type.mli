open Ast
open LilyFront.Error

val token_to_type : ast -> (lily_type, error_id) result
