open Ast
open LilyFront.Error

val token_to_type : 'a ast -> (lily_type, error_id) result
