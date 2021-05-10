open Ast
open Binop
open LilyFront.Error

module Token = LilyFront.Token

module ParserUtil = struct
    let next_token ast =
        ast.pos <- ast.pos + 1;
        ast.current_token <- CCVector.get ast.stream.tok ast.pos;
        ast.current_location <- CCVector.get ast.stream.loc ast.pos;
        ()

    let get_next_token ast = CCVector.get ast.stream.tok (ast.pos+1)

    let get_previous_token ast = CCVector.get ast.stream.tok (ast.pos-1)
end

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
      let left = parse_expr_value (ParserUtil.get_previous_token ast) in
      let right = parse_expr_value (ParserUtil.get_next_token ast) in
      match (left, right) with
      | (Ok x1, Ok x2) -> (match token_to_binop ast.current_token with
                           | Ok b -> Ok (Expr (ExprBinop (x1,b,x2)))
                           | Error e -> Error e)
      | _ -> Error (ErrorIdInvalidValue)
end

module ParseStmt = struct
end

let parser ast =
  match ast.current_token with
  | Operator OperatorPlus -> Some (ParseExpr.parse_binop_operator ast)
  | Operator OperatorMinus -> Some (ParseExpr.parse_binop_operator ast)
  | Operator OperatorStar -> Some (ParseExpr.parse_binop_operator ast)
  | Operator OperatorSlash -> Some (ParseExpr.parse_binop_operator ast)
  | Operator OperatorPercentage -> Some (ParseExpr.parse_binop_operator ast)
  | Operator OperatorHat -> Some (ParseExpr.parse_binop_operator ast)
  | _ -> None

let run_parser ast =
  let rec loop ast =
    if ast.pos < (CCVector.length (ast.stream.tok))-1 then
      match parser ast with
      | Some (Error _) -> Printf.printf "error\n"
      | Some (Ok p) -> (Printf.printf "%s\n" (ast_kind_to_str (p));
                        ParserUtil.next_token ast;
                        loop (ast))
      | None -> (ParserUtil.next_token ast;
                 loop (ast)) in
  loop (ast)
