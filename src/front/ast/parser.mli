open Ast
open LilyFront.Token
open LilyFront.Error

module ParserUtil : sig
    val next_token : 'a ast -> unit
    val get_next_token : 'a ast -> 'a token
    val get_previous_token : 'a ast -> 'a token
end

module ParseExpr : sig
    val parse_expr_value : 'a token -> ('a expr, error_id) result
    val parse_binop_operator : 'a ast -> ('a ast_kind, error_id) result
    (*val parse_expr : 'a ast -> 'a ast_kind*)
end

module ParseStmt : sig
end

val parser : 'a ast -> (('a ast_kind, error_id) result) option

val run_parser : 'a ast -> unit
