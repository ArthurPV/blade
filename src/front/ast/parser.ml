open Ast
open Binop
open Unary
open LilyFront.Error

module Token = LilyFront.Token

module ParserUtil = struct
    let next_token ast =
        ast.pos <- ast.pos + 1;
        ast.current_token <- CCVector.get ast.stream.tok ast.pos;
        ast.current_location <- CCVector.get ast.stream.loc ast.pos;
        ()

    let get_next_token ast = 
        if ast.pos+1 >= (CCVector.length (ast.stream.tok))-1 then
            Ok (CCVector.get ast.stream.tok (ast.pos+1))
        else 
            Error (ErrorIdUnexpectedToken)

    let get_previous_token ast = 
        if ast.pos-1 >= 0 then
            Ok (CCVector.get ast.stream.tok (ast.pos-1))
        else
            Error (ErrorIdUnexpectedToken)
end

module ParseExpr = struct
    let parse_expr_value tok =
        match tok with
        | Ok (Token.Keyword KeywordTrue) -> Ok (ExprLiteral (LiteralBool true))
        | Ok (Token.Keyword KeywordFalse) -> Ok (ExprLiteral (LiteralBool false))
        | Ok (Token.Literal LiteralInt (v,_)) -> Ok (ExprLiteral (LiteralInt v))
        | Ok (Token.Literal LiteralFloat (v,_)) -> Ok (ExprLiteral (LiteralFloat v))
        | Ok (Token.Literal LiteralString s) -> Ok (ExprLiteral (LiteralString s))
        | Ok (Token.Literal LiteralChar c) -> Ok (ExprLiteral (LiteralChar c))
        | _ -> Error (ErrorIdInvalidValue)

    (* + - not *)
    let parse_unary ast = 
        let right = parse_expr_value (ParserUtil.get_next_token ast) in 
        match right with
        | Ok x1 -> (match token_to_unary ast.current_token with
                    | Ok b -> Ok (Expr (ExprUnary (x1, b)))
                    | Error e -> Error e)
        | _ -> Error (ErrorIdInvalidValue)

    (* + - * / % ^ *)
    let parse_binop_operator ast =
      let left = parse_expr_value (ParserUtil.get_previous_token ast) in
      let right = parse_expr_value (ParserUtil.get_next_token ast) in
      if ast.current_token = Operator (OperatorPlus) || ast.current_token = Operator (OperatorMinus) then 
          match (left,right) with
          | (Ok x1, Ok x2) -> (match token_to_binop ast.current_token with
                               | Ok b -> Ok (Expr (ExprBinop (x1,b,x2)))
                               | Error e -> Error e)
          | (Error _,Ok _) -> parse_unary ast
          | _ -> Error (ErrorIdInvalidValue)
      else
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
  | Keyword KeywordNot -> Some (ParseExpr.parse_unary ast)
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
