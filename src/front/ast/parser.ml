open Ast
open Binop
(*open Identifier*)
open Stream
open Unary
open Default_type
open LilyFront.Error

module Token = LilyFront.Token

module ParserUtil = struct
    let next_token ast =
        if ast.pos < (CCVector.length (ast.stream.tok))-1 then
            (ast.pos <- ast.pos + 1;
             ast.current_token <- CCVector.get ast.stream.tok ast.pos;
             ast.current_location <- CCVector.get ast.stream.loc ast.pos;
             ())
        else ()

    let previous_token ast = 
        ast.pos <- ast.pos - 1;
        ast.current_token <- CCVector.get ast.stream.tok ast.pos;
        ast.current_location <- CCVector.get ast.stream.loc ast.pos;
        ()

    let get_next_token ast = 
        if ast.pos+1 >= (CCVector.length (ast.stream.tok))-1 then
            Ok (CCVector.get ast.stream.tok (ast.pos+1))
        else 
            Error (ErrorIdMissToken)

    let get_previous_token ast = 
        if ast.pos-1 >= 0 then
            Ok (CCVector.get ast.stream.tok (ast.pos-1))
        else
            Error (ErrorIdMissToken)

    let assert_eq_token ast tok = 
        if ast.current_token = tok then true
        else false
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
                    | Ok b -> (Ok (ExprUnary (x1, b)))
                    | Error e -> Error e)
        | _ -> Error (ErrorIdInvalidValue)

    (* + - * / % ^ *)
    let parse_binop_operator ast =
      let left = parse_expr_value (ParserUtil.get_previous_token ast) in
      let right = parse_expr_value (ParserUtil.get_next_token ast) in
      if ast.current_token = Operator (OperatorPlus) || ast.current_token = Operator (OperatorMinus) then 
          match (left,right) with
          | (Ok x1, Ok x2) -> (match token_to_binop ast.current_token with
                               | Ok b -> Ok (ExprBinop (x1,b,x2))
                               | Error e -> Error e)
          | (Error _,Ok _) -> parse_unary ast
          | _ -> Error (ErrorIdInvalidValue)
      else
          match (left, right) with
          | (Ok x1, Ok x2) -> (match token_to_binop ast.current_token with
                               | Ok b -> Ok (ExprBinop (x1,b,x2))
                               | Error e -> Error e)
          | _ -> Error (ErrorIdInvalidValue)

    let parse_end_line ast =
        if ast.pos = (CCVector.length (ast.stream.tok))-1 then ()
        else
            match ast.current_token with
            | Token.Separator SeparatorNewline -> ParserUtil.next_token ast;
            | Token.Comment CommentDoc _ 
            | Token.Comment CommentOneLine 
            | Token.Comment CommentMultiLine 
            -> ()
            | _ -> print_error (ErrorIdExpectedNewLine) ast.current_location.line ast.current_location.col ast.filename

    (* a = <expr> *)
    (* sum :: <type> -> <type> -> <return type> (like in Haskell) *)
    let parse_identifier ast = 
        match ast.current_token with
        | Identifier s -> Ok (ExprIdentifier (s))
        | _ -> Error (ErrorIdUnexpectedIdentifier)

    let read_expr ast = 
        match ast.current_token with
        | Token.Operator OperatorPlus -> parse_binop_operator ast
        | Token.Operator OperatorMinus -> parse_binop_operator ast
        | Token.Operator OperatorStar -> parse_binop_operator ast
        | Token.Operator OperatorSlash -> parse_binop_operator ast
        | Token.Operator OperatorPercentage -> parse_binop_operator ast
        | Token.Operator OperatorHat -> parse_binop_operator ast
        | Token.Keyword KeywordNot -> parse_unary ast
        | Token.Identifier s -> parse_identifier ast
        | Token.Keyword KeywordTrue -> Ok (ExprLiteral (LiteralBool (true)))
        | Token.Keyword KeywordFalse -> Ok (ExprLiteral (LiteralBool (false)))
        | Token.Literal LiteralInt (v,_) -> Ok (ExprLiteral (LiteralInt (v)))
        | Token.Literal LiteralFloat (v,_) -> Ok (ExprLiteral (LiteralFloat (v)))
        | Token.Literal LiteralChar (c) -> Ok (ExprLiteral (LiteralChar (c)))
        | Token.Literal LiteralString (s) -> Ok (ExprLiteral (LiteralString (s)))
        | _ -> Error (ErrorIdUnexpectedExpr)

    (* var <id> *)
    (* var <id> :: <type> *)
    (* var <id> = <expr> *)
    (* var <id> :: <type> = <expr> *)
    let parse_var ast = 
        ParserUtil.next_token ast;
        match ast.current_token with
        | Identifier s -> (let id = ExprIdentifier s in
                           ParserUtil.next_token ast;

                           if ast.current_token = (Separator SeparatorColonColon) then
                               (ParserUtil.next_token ast;
                                match token_to_type ast with
                                | Ok t -> (let tp = t in 
                                           ParserUtil.next_token ast;
                                           if token_to_binop ast.current_token = (Ok BinopAssign) then
                                               (ParserUtil.next_token ast;
                                                match read_expr ast with
                                                | Ok expr -> (ParserUtil.next_token ast;
                                                              parse_end_line ast;
                                                              (Ok (ExprVarDeclareTypeAndAssign (id, tp, expr))))
                                                | Error e -> Error e)
                                           else (parse_end_line ast;
                                                 Ok (ExprVarDefineType (id, tp))))
                                | Error e -> Error e)

                           else if token_to_binop ast.current_token = (Ok BinopAssign) then
                               (ParserUtil.next_token ast;
                                match read_expr ast with
                                | Ok expr -> (ParserUtil.next_token ast;
                                              parse_end_line ast;
                                              (Ok (ExprVarAssign (id, expr))))
                                | Error e -> Error e)

                           else (parse_end_line ast;
                                 Ok (ExprVarDefine (id))))
        | _ -> Error (ErrorIdMissIdentifier)

    (* const <id> *)
    (* const <id> :: <type> *)
    (* const <id> = <expr> *)
    (* const <id> :: <type> = <expr> *)
    let parse_const ast = 
        ParserUtil.next_token ast;
        match ast.current_token with
        | Identifier s -> (let id = ExprIdentifier s in
                           ParserUtil.next_token ast;

                           if ast.current_token = (Separator SeparatorColonColon) then
                               (ParserUtil.next_token ast;
                                match token_to_type ast with
                                | Ok t -> (let tp = t in 
                                           ParserUtil.next_token ast;
                                           if token_to_binop ast.current_token = (Ok BinopAssign) then
                                               (ParserUtil.next_token ast;
                                                match read_expr ast with
                                                | Ok expr -> (ParserUtil.next_token ast;
                                                              parse_end_line ast;
                                                              (Ok (ExprConstDeclareTypeAndAssign (id, tp, expr))))
                                                | Error e -> Error e)
                                           else (parse_end_line ast;
                                                 Ok (ExprConstDefineType (id, tp))))
                                | Error e -> Error e)

                           else if token_to_binop ast.current_token = (Ok BinopAssign) then
                               (ParserUtil.next_token ast;
                                match read_expr ast with
                                | Ok expr -> (ParserUtil.next_token ast;
                                              parse_end_line ast;
                                              (Ok (ExprConstAssign (id, expr))))
                                | Error e -> Error e)

                           else (parse_end_line ast;
                                 Ok (ExprConstDefine (id))))
        | _ -> Error (ErrorIdMissIdentifier)
end

module ParseStmt = struct
end

let parser ast =
    match ast.current_token with
    | Token.Operator OperatorPlus
    | Token.Operator OperatorMinus
    | Token.Operator OperatorStar
    | Token.Operator OperatorSlash
    | Token.Operator OperatorPercentage
    | Token.Operator OperatorHat
    -> (match ParseExpr.parse_binop_operator ast with
        | Ok b -> Ok (Expr (b))
        | Error e -> Error e)
    | Token.Keyword KeywordNot -> (match ParseExpr.parse_unary ast with
                                   | Ok u -> Ok (Expr (u))
                                   | Error e -> Error e)
    | Token.Keyword KeywordVar -> (match ParseExpr.parse_var ast with
                                   | Ok v -> Ok (Expr (v))
                                   | Error e -> Error e)
    | Token.Keyword KeywordConst -> (match ParseExpr.parse_const ast with
                                     | Ok v -> Ok (Expr (v))
                                     | Error e -> Error e)
    | Token.Separator SeparatorNewline -> Ok (Expr (ExprNewline))
    | Token.Comment CommentOneLine -> (ParserUtil.next_token ast;
                                       ParseExpr.parse_end_line ast;
                                       Ok (Expr (ExprCommentOneLine)))
    | Token.Comment CommentMultiLine -> (ParserUtil.next_token ast;
                                         ParseExpr.parse_end_line ast;
                                         Ok (Expr (ExprCommentMultiLine)))
    | Token.Comment CommentDoc s -> (ParserUtil.next_token ast;
                                     ParseExpr.parse_end_line ast;
                                     Ok (Expr (ExprCommentDoc s)))
    | _ -> Error (ErrorIdUnexpectedAst)

let run_parser ast =
    let rec loop ast =
        if ast.pos < (CCVector.length (ast.stream.tok))-1 then
        match parser ast with
        | Error e -> print_error e ast.current_location.line ast.current_location.col ast.filename
        | Ok (Expr (ExprNewline)) -> ParserUtil.next_token ast; loop (ast)
        | Ok p -> (Printf.printf "%s\n" (ast_kind_to_str (p));
                   push_ast (new_stream_ast) p;
                   loop (ast)) in
    loop (ast)
