(*open Ast

val next_token : 'a ast -> unit

val get_next_token : 'a ast -> unit

val get_previous_token : 'a ast -> unit

module ParseExpr : sig
  val parse_binop : 'a ast_kind -> 
end

module ParseStmt : sig
end
 *)
