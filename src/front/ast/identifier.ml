open Ast

let get_identifier ast =
  match ast.current_token with
  | Identifier s -> Some (Expr (ExprIdentifier s))
  | _ -> None
