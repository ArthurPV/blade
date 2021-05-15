open Ast
open LilyFront.Token
open LilyFront.Error

module ParserUtil : sig
    val next_token : ast -> unit
    val get_next_token : ast -> (token, error_id) result
    val get_previous_token : ast -> (token, error_id) result
end

module ParseExpr : sig
    val parse_expr_value : (token, error_id) result -> (expr, error_id) result
    val parse_binop_operator : ast -> (ast_kind, error_id) result
    val parse_newline : ast -> bool
    (*val parse_expr : 'a ast -> 'a ast_kind*)
end

module ParseStmt : sig
end

val parser : ast -> ((ast_kind, error_id) result) option

val run_parser : ast -> unit
