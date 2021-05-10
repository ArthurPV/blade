open Ast
open Binop
open LilyFront.Error

module Token = LilyFront.Token

let next_token ast =
    ast.pos <- ast.pos + 1;
    ast.current_token <- CCVector.get ast.stream.tok ast.pos;
    ast.current_location <- CCVector.get ast.stream.loc ast.pos;
    ()

let get_next_token ast = CCVector.get ast.stream.tok (ast.pos+1)

let get_previous_token ast = CCVector.get ast.stream.tok (ast.pos-1)

module ParseExpr = struct
    let parse_expr_value tok =
        match tok with
        | Token.Keyword KeywordTrue -> Ok (ExprLiteral (LiteralBool true))
        | Token.Keyword KeywordFalse -> Ok (ExprLiteral (LiteralBool false))
        | Token.Literal LiteralInt (v,_) -> Ok (ExprLiteral (LiteralInt v))
        | Token.Literal LiteralFloat (v,_) -> Ok (ExprLiteral (LiteralFloat v))
        | Token.Literal LiteralString s -> Ok (ExprLiteral (LiteralString s))
        | Token.Literal LiteralChar c -> Ok (ExprLiteral (LiteralChar c))
        | _ -> Error (ErrorIdInvalidValue)

    (* + - * / % ^ *)
    let parse_binop_operator ast =
      let left = parse_expr_value (get_previous_token ast) in
      let right = parse_expr_value (get_next_token ast) in
      match (left, right) with
      | (Ok x1, Ok x2) -> (match token_to_binop ast.current_token with
                           | Ok b -> (next_token ast;
                                      next_token ast;
                                      Ok (Expr (ExprBinop (x1,b,x2))))
                           | Error e -> Error e)
      | _ -> Error (ErrorIdInvalidValue)
end

module ParseStmt = struct
end
