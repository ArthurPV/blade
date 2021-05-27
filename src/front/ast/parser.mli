open Ast
open LilyFront.Token
open LilyFront.Error

module ParserUtil : sig
    val next_token : ast -> unit
    val previous_token : ast -> unit
    val get_next_token : ast -> (token, error_id) result
    val get_previous_token : ast -> (token, error_id) result
    val is_end_line : ast -> bool
    val assert_eq_token : ast -> token -> bool
end

module ParseExpr : sig
    val parse_expr_value : (token, error_id) result -> (expr, error_id) result
    val parse_unary : ast -> (expr, error_id) result
    val parse_binop_operator : ast -> (expr, error_id) result
    val parse_end_line : ast -> unit
    val parse_identifier : ast -> (expr, error_id) result
    val read_expr : ast -> (expr, error_id) result
    val parse_assign : ast -> expr -> (expr, error_id) result
    val parse_fun_define : ast -> expr -> (expr,error_id) result
    val parse_fun_call : ast -> id:expr -> (expr array) -> (expr, 'b) result
    val parse_expr_identifier : ast -> (expr, error_id) result
    val parse_var : ast -> (expr, error_id) result
    val parse_const : ast -> (expr, error_id) result
    val parse_fun : ast -> (expr, error_id) result
    val parse_async_fun : ast -> (expr, error_id) result
    val parse_import : ast -> (expr, error_id) result
    val parse_share : ast -> (expr, error_id) result
    val parse_body : ast -> (expr, error_id) result
    val parse_await : ast -> (expr, error_id) result
    val parse_anonymous_fun : ast -> (expr, error_id) result
    val parse_array : ast -> (expr, error_id) result
    val parse_tuple : ast -> (expr, error_id) result
    val parse_pub : ast -> (expr, error_id) result
    val parse_class_init : ast -> (expr, error_id) result
    val parse_explicit_module : ast -> (expr, error_id) result
    val parse_explicit_class : ast -> (expr, error_id) result
    val parse_explicit : ast -> (expr, error_id) result
    val parse_module : ast -> (expr, error_id) result
    val parse_class_inherit : ast -> (expr, error_id) result
    val parse_class : ast -> (expr, error_id) result
    val parse_call_class : ast -> (expr, error_id) result
    val parse_type : ast -> (expr, error_id) result
    val parse_call_field_type : ast -> (expr, error_id) result
    val parse_data_constructor : ast -> (expr, error_id) result
    val parse_data : ast -> (expr, error_id) result
    val parse_macro : ast -> (expr, error_id) result
end

module ParseStmt : sig
    val parse_stmt_if : ast -> (stmt, error_id) result
    val parse_stmt_switch : ast -> (stmt, error_id) result
    val parse_stmt_break : ast -> (stmt, error_id) result
    val parse_stmt_next : ast -> (stmt, error_id) result
    val parse_stmt_while : ast -> (stmt, error_id) result
    val parse_stmt_for : ast -> (stmt, error_id) result
    val parse_stmt_loop : ast -> (stmt, error_id) result
    val parse_stmt_return : ast -> (stmt, error_id) result
end

val parser : ast -> (ast_kind, error_id) result

val run_parser : ast -> unit
