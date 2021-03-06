open Ast
open Binop
(*open Identifier*)
(*open Precedence*)
open Stream
open Unary
open Default_type
open LilyFront.Error

module Token = LilyFront.Token

(* TODO: Finish read_expr and parse all unary and binop *)

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
        if ast.pos+1 > (CCVector.length (ast.stream.tok))-1 then
            Error (ErrorIdMissToken)
        else 
            Ok (CCVector.get ast.stream.tok (ast.pos+1))

    let get_previous_token ast = 
        if ast.pos-1 > 0 then
            Ok (CCVector.get ast.stream.tok (ast.pos-1))
        else
            Error (ErrorIdMissToken)

    let is_end_line ast = 
        if ast.pos = (CCVector.length (ast.stream.tok))-1 then true
        else
        (match ast.current_token with
        | Token.Comment CommentOneLine -> true
        | Token.Comment CommentMultiLine -> true
        | Token.Comment CommentDoc _ -> true
        | Token.Separator SeparatorNewline -> true
        | _ -> false)

    let assert_eq_token ast tok = 
        if ast.current_token = tok then true
        else false

    let rec skip_newline ast = 
        if ast.current_token = (Token.Separator SeparatorNewline) then
            (next_token ast;
             skip_newline (ast))
        else ()

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
        | Ok r -> (
            match token_to_unary ast.current_token with
            | Ok u -> (Ok (ExprUnary {unary = u;
                                      right = r}))
            | Error e -> Error e)
        | _ -> Error (ErrorIdInvalidValue)

    (* + - * / % ^ *)
    let parse_binop_operator ast =
      let left = parse_expr_value (ParserUtil.get_previous_token ast) in
      let right = parse_expr_value (ParserUtil.get_next_token ast) in
      if ast.current_token = Operator (OperatorPlus) || ast.current_token = Operator (OperatorMinus) then 
          match (left,right) with
          | (Ok l, Ok r) -> (
              match token_to_binop ast.current_token with
              | Ok b -> Ok (ExprBinop {left = l; 
                                       binop = b; 
                                       right = r})
              | Error e -> Error e)
          | (Error _,Ok _) -> parse_unary ast
          | _ -> Error (ErrorIdInvalidValue)
      else
          match (left, right) with
          | (Ok l, Ok r) -> (
              match token_to_binop ast.current_token with
              | Ok b -> Ok (ExprBinop {left = l;
                                       binop = b; 
                                       right = r})
              | Error e -> Error e)
          | _ -> Error (ErrorIdInvalidValue)

    (* = += -= *= /= %= ^= *)
    let parse_binop_assign ast = 
        Error (ErrorIdMissIdentifier)

    (* and or *)
    let parse_binop_logical ast = 
        Error (ErrorIdMissIdentifier)

    let parse_end_line ast =
        if ast.pos = (CCVector.length (ast.stream.tok))-1 then ()
        else
            match ast.current_token with
            | Token.Separator SeparatorNewline -> ParserUtil.next_token ast;
            | Token.Comment CommentDoc _ 
            | Token.Comment CommentOneLine 
            | Token.Comment CommentMultiLine 
            -> ()
            | _ -> print_error ErrorIdSyntaxError 
                               ~line:ast.current_location.line 
                               ~col:ast.current_location.col 
                               ast.filename

    let parse_identifier ast = 
        match ast.current_token with
        | Token.Identifier s -> Ok (ExprIdentifier (s))
        | _ -> Error (ErrorIdUnexpectedIdentifier)

    let read_expr ast = 
        match ast.current_token with
        | Token.Separator SeparatorLeftParen
        | Token.Operator OperatorPlus
        | Token.Operator OperatorMinus
        | Token.Operator OperatorStar
        | Token.Operator OperatorSlash
        | Token.Operator OperatorPercentage
        | Token.Operator OperatorHat
        | Token.Operator OperatorLeftShift
        | Token.Operator OperatorRightShift
        | Token.Operator OperatorLeftShiftEq
        | Token.Operator OperatorRightShiftEq
        -> (
            (* make in precedence order *)
            (*ParserUtil.next_token ast;
            let precedence_order = CCVector.create () in
            let p = ref 1 in
            let rec loop ast =
                if !p < 9 then
                    match get_precedence ast.current_token with
                    | Some precedence -> (
                        if precedence = 1 && precedence = !p then 
                            (while ast.current_token <> Token.Separator SeparatorRightParen do
                                CCVector.push precedence_order ast.current_token;
                                ParserUtil.next_token ast;
                            done;
                            CCVector.push precedence_order ast.current_token;
                            ParserUtil.next_token ast;
                            loop (ast))
                        else if precedence = 2 && precedence = !p then 
                            print_error (ErrorIdUnexpectedToken "not")
                            ~line:ast.current_location.line
                            ~col:ast.current_location.col
                            ast.filename
                        else if precedence = 3 && precedence = !p then (
                            match ParserUtil.get_previous_token ast with
                            | Ok t -> CCVector.push precedence_order t
                            | Error e -> print_error e
                            ~line:ast.current_location.line
                            ~col:ast.current_location.col
                            ast.filename;
                            CCVector.push precedence_order ast.current_token;
                            ParserUtil.next_token ast;
                            CCVector.push precedence_order ast.current_token;
                            ParserUtil.next_token ast)
                        else if precedence = 4 && precedence = !p then ()
                        else ())
                    | None -> print_error (ErrorIdUnexpectedToken "")
                    ~line:ast.current_location.line
                    ~col:ast.current_location.col
                    ast.filename in
            loop (ast)*)
            parse_unary ast)
        (*| Token.Operator OperatorPlus -> parse_binop_operator ast
        | Token.Operator OperatorMinus -> parse_binop_operator ast
        | Token.Operator OperatorStar -> parse_binop_operator ast
        | Token.Operator OperatorSlash -> parse_binop_operator ast
        | Token.Operator OperatorPercentage -> parse_binop_operator ast
        | Token.Operator OperatorHat -> parse_binop_operator ast*)
        | Token.Keyword KeywordNot -> parse_unary ast
        | Token.Identifier s -> parse_identifier ast
        | Token.Keyword KeywordTrue -> Ok (ExprLiteral (LiteralBool (true)))
        | Token.Keyword KeywordFalse -> Ok (ExprLiteral (LiteralBool (false)))
        | Token.Literal LiteralInt (v,_) -> Ok (ExprLiteral (LiteralInt (v)))
        | Token.Literal LiteralFloat (v,_) -> Ok (ExprLiteral (LiteralFloat (v)))
        | Token.Literal LiteralChar (c) -> Ok (ExprLiteral (LiteralChar (c)))
        | Token.Literal LiteralString (s) -> Ok (ExprLiteral (LiteralString (s)))
        | _ -> Error (ErrorIdUnexpectedExpr)

    (* TODO: error *)
    (* sum :: <type> -> <type> -> <return type> (like in Haskell) *)
    let parse_fun_define ast id = 
        ParserUtil.next_token ast;
        let args = CCVector.create () in
        let rec loop ast = 
            if ParserUtil.is_end_line ast = false then
                (match token_to_type ast with
                 | Error e -> print_error e 
                 ~line:ast.current_location.line 
                 ~col:ast.current_location.col 
                 ast.filename
                 | Ok ty -> (
                     ParserUtil.next_token ast;
                     ParserUtil.skip_newline ast;
                     if ast.current_token <> (Token.Separator SeparatorArrow) && 
                     ParserUtil.is_end_line ast = false then
                         (Printf.printf "pos: %d\n" ast.pos;
                          print_error ErrorIdSyntaxError 
                          ~line:ast.current_location.line 
                          ~col:ast.current_location.col 
                          ast.filename)
                     else
                         (CCVector.push args ty;
                          ParserUtil.next_token ast;
                          ParserUtil.skip_newline ast;
                          loop (ast)))) in
        loop (ast);
        match token_to_type ast with
        | Ok ret -> Ok (ExprFunDefine {id = id; 
                                       tp = args;
                                       ret = ret})
        | Error e -> Error e

    
    (* fun <id> <arg>,<arg>... = <body> *)
    let parse_fun_declare ast = 
        Error (ErrorIdMissToken)

    (* sum(<expr>, <expr>) *)
    let parse_fun_call ast id = 
        ParserUtil.next_token ast;
        let args = CCVector.create () in
        let rec loop ast =
            if ast.current_token = (Token.Separator SeparatorRightParen) then
                ParserUtil.next_token ast
            else 
                ParserUtil.skip_newline ast;

                (match read_expr ast with
                 | Error e -> (
                     print_error e 
                     ~line:ast.current_location.line
                     ~col:ast.current_location.col 
                     ast.filename)
                 | Ok expr -> (
                     CCVector.push args expr;
                     ParserUtil.next_token ast;
                     ParserUtil.skip_newline ast;
                     if ast.current_token <> (Token.Separator SeparatorComma) &&
                     ast.current_token <> (Token.Separator SeparatorRightParen) then
                         (print_error 
                         (ErrorIdExpectedToken (Token.token_to_str (Token.Separator SeparatorRightParen)))
                         ~line:ast.current_location.line
                         ~col:ast.current_location.col
                         ast.filename)
                     else if ast.current_token = (Token.Separator SeparatorRightParen) then 
                         (ParserUtil.next_token ast;
                          parse_end_line ast)
                     else
                         (ParserUtil.next_token ast; 
                          loop (ast)))) in
        loop (ast);
    
      if ast.current_token = (Token.Separator SeparatorRightParen) then
          (match ParserUtil.get_previous_token ast with
          | Ok (Token.Separator SeparatorComma) -> (
              print_error ErrorIdUnexpectedExpr
              ~line:ast.current_location.line
              ~col:ast.current_location.col
              ast.filename)
          | _ -> parse_end_line ast);

    (Ok (ExprFunCall {id = id; 
                      args = args}))


let parse_expr_identifier ast = 
        match ast.current_token with
        | Token.Identifier s -> (
            let id = ExprIdentifier s in 
            ParserUtil.next_token ast;
            if ast.current_token = Token.Separator SeparatorColonColon then parse_fun_define ast id
            else if ast.current_token = Token.Separator SeparatorLeftParen then parse_fun_call ast id
            else (Error (ErrorIdSyntaxError)))
        | _ -> Error (ErrorIdMissIdentifier)

    (* var <id> *)
    (* var <id> :: <type> *)
    (* var <id> = <expr> *)
    (* var <id> :: <type> = <expr> *)
    let parse_var ast = 
        ParserUtil.next_token ast;
        match ast.current_token with
        | Token.Identifier s -> (
            let id = ExprIdentifier s in
            ParserUtil.next_token ast;

            if ast.current_token = (Separator SeparatorColonColon) then
                (ParserUtil.next_token ast;
                 match token_to_type ast with
                 | Ok t -> (
                     let tp = t in 
                     ParserUtil.next_token ast;
                     if token_to_binop ast.current_token = (Ok BinopAssign) then
                         (ParserUtil.next_token ast;
                          match read_expr ast with
                          | Ok expr -> (
                              ParserUtil.next_token ast;
                              parse_end_line ast;
                              (Ok (ExprVarDeclareTypeAndAssign {id = id; 
                                                                tp = tp;
                                                                expr = expr})))
                          | Error e -> Error e)

                     else if ParserUtil.is_end_line ast = false then  
                         Error (ErrorIdExpectedToken (Token.token_to_str (Token.Operator OperatorEq)))
                     else (
                         parse_end_line ast;
                         Ok (ExprVarDefineType {id = id; 
                                                tp = tp})))
                 | Error e -> Error e)

                     else if token_to_binop ast.current_token = (Ok BinopAssign) then
                         (ParserUtil.next_token ast;
                          match read_expr ast with
                          | Ok expr -> (
                              ParserUtil.next_token ast;
                              parse_end_line ast;
                              (Ok (ExprVarAssign {id = id; 
                                                  expr = expr})))
                          | Error e -> Error e)

            else if ParserUtil.is_end_line ast = false then
                Error (ErrorIdExpectedToken (Token.token_to_str (Token.Operator OperatorEq)))
            else 
                (parse_end_line ast;
                 Ok (ExprVarDefine (id))))
        | _ -> Error (ErrorIdMissIdentifier)

    (* const <id> *)
    (* const <id> :: <type> *)
    (* const <id> = <expr> *)
    (* const <id> :: <type> = <expr> *)
    let parse_const ast = 
        ParserUtil.next_token ast;
        match ast.current_token with
        | Token.Identifier s -> (let id = ExprIdentifier s in
                           ParserUtil.next_token ast;

                           if ast.current_token = (Separator SeparatorColonColon) then
                               (ParserUtil.next_token ast;
                                match token_to_type ast with
                                | Ok t -> (
                                    let tp = t in 
                                    ParserUtil.next_token ast;
                                    if token_to_binop ast.current_token = (Ok BinopAssign) then
                                        (ParserUtil.next_token ast;
                                         match read_expr ast with
                                         | Ok expr -> (
                                             ParserUtil.next_token ast;
                                             parse_end_line ast;
                                             (Ok (ExprConstDeclareTypeAndAssign {id = id; 
                                                                                 tp = tp; 
                                                                                 expr = expr})))
                                         | Error e -> Error e)

                                    else if ParserUtil.is_end_line ast = false then  
                                        Error (ErrorIdExpectedToken (Token.token_to_str (Token.Operator OperatorEq)))
                                    else (parse_end_line ast;
                                          Ok (ExprConstDefineType {id = id; 
                                                                   tp = tp})))
                                | Error e -> Error e)

                           else if token_to_binop ast.current_token = (Ok BinopAssign) then
                               (ParserUtil.next_token ast;
                                match read_expr ast with
                                | Ok expr -> (
                                    ParserUtil.next_token ast;
                                    parse_end_line ast;
                                    (Ok (ExprConstAssign {id = id; 
                                                          expr = expr})))
                                | Error e -> Error e)

                             else if ParserUtil.is_end_line ast = false then
                                 Error (ErrorIdExpectedToken (Token.token_to_str (Token.Operator OperatorEq)))

                           else (parse_end_line ast;
                                 Ok (ExprConstDefine (id))))
        | _ -> Error (ErrorIdMissIdentifier)

    let parse_async_fun ast = 
        Error (ErrorIdMissIdentifier)

    (* import <expr> *)
    let parse_import ast = 
        ParserUtil.next_token ast;
        match ast.current_token with
        | Token.Literal LiteralString(s) -> (
            ParserUtil.next_token ast;
            Ok (ExprImport (LiteralString (s))))
        | _ -> Error (ErrorIdUnexpectedImportValue)

    let parse_share ast = 
        Error (ErrorIdMissIdentifier)

    let parse_body ast = 
        Error (ErrorIdMissIdentifier)

    (* await <expr> *)
    let parse_await ast = 
        ParserUtil.next_token ast;
        match read_expr ast with
        | Ok expr -> Ok (ExprAwait (expr))
        | Error e -> Error e

    let parse_anonymous_fun ast = 
        Error (ErrorIdMissIdentifier)

    (* [<expr>, <expr>, <expr>]  *)
    let parse_array ast =
        ParserUtil.next_token ast; 
        let expr_arr = CCVector.create () in
        let rec loop ast = 
            match read_expr ast with
            | Ok expr -> (
                CCVector.push expr_arr expr;
                ParserUtil.next_token ast;
                match ast.current_token with
                | Token.Separator SeparatorComma -> (
                    ParserUtil.next_token ast;
                    loop (ast))
                | Token.Separator SeparatorRightHook -> (
                    ParserUtil.next_token ast;
                    parse_end_line ast;
                    ())
                | _ -> print_error ErrorIdUnexpectedExpr ~line:ast.current_location.line ~col:ast.current_location.col ast.filename)
            | Error e -> print_error e ~line:ast.current_location.line ~col:ast.current_location.col ast.filename in
        loop (ast);
        Ok (ExprArray {items = expr_arr})

    (* (<expr>, <expr>, <expr>)  *)
    let parse_tuple ast = 
        ParserUtil.next_token ast; 
        let expr_arr = CCVector.create () in
        let rec loop ast = 
            match read_expr ast with
            | Ok expr -> (
                CCVector.push expr_arr expr;
                ParserUtil.next_token ast;
                match ast.current_token with
                | Token.Separator SeparatorComma -> (
                    ParserUtil.next_token ast;
                    loop (ast))
                | Token.Separator SeparatorRightParen -> (
                    ParserUtil.next_token ast;
                    parse_end_line ast;
                    ())
                | _ -> print_error ErrorIdUnexpectedExpr ~line:ast.current_location.line ~col:ast.current_location.col ast.filename)
            | Error e -> print_error e ~line:ast.current_location.line ~col:ast.current_location.col ast.filename in
        loop (ast);
        Ok (ExprTuple {items = expr_arr})

    let parse_pub ast = 
        match ParserUtil.get_next_token ast with
        | Ok (Token.Keyword KeywordFun) -> (
            ParserUtil.next_token ast;
            parse_fun_declare ast)
        | Ok _ -> Error (ErrorIdMissToken)
        | Error e -> Error e

    (* init <arg>,<arg>,... = <body> end *)
    let parse_class_init ast = 
        Error (ErrorIdMissIdentifier)

    let parse_explicit_module ast = 
        Error (ErrorIdMissIdentifier)

    let parse_explicit_class ast = 
        Error (ErrorIdMissIdentifier)

    (* explicit <class> <module> *)
    let parse_explicit ast = 
        Error (ErrorIdMissIdentifier)

    let parse_module ast = 
        Error (ErrorIdMissIdentifier)

    let parse_class_inherit ast = 
        Error (ErrorIdMissIdentifier)

    let parse_class ast = 
        Error (ErrorIdMissIdentifier)

    let parse_call_class ast = 
        Error (ErrorIdMissIdentifier)

    let parse_type ast = 
        Error (ErrorIdMissIdentifier)

    let parse_call_field_type ast = 
        Error (ErrorIdMissIdentifier)

    let parse_data_constructor ast = 
        Error (ErrorIdMissIdentifier)
    
    (* 
       data Person = 
           Name: string,
           Age: u8,
       end 
    *)
    let parse_data ast = 
        Error (ErrorIdMissIdentifier)

    let parse_macro ast = 
        Error (ErrorIdMissIdentifier)
end

module ParseStmt = struct
    let parse_stmt_if ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_switch ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_break ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_next ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_while ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_for ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_loop ast = 
        Error (ErrorIdMissIdentifier)

    let parse_stmt_return ast = 
        Error (ErrorIdMissIdentifier)
end

let parser ast =
    ParserUtil.skip_newline ast;
    match ast.current_token with
    | Token.Operator OperatorPlus
    | Token.Operator OperatorMinus
    | Token.Operator OperatorStar
    | Token.Operator OperatorSlash
    | Token.Operator OperatorPercentage
    | Token.Operator OperatorHat
    -> (
        match ParseExpr.parse_binop_operator ast with
        | Ok b -> Ok (Expr (b))
        | Error e -> Error e)
    | Token.Operator OperatorEq
    | Token.Operator OperatorPlusEq
    | Token.Operator OperatorMinusEq
    | Token.Operator OperatorStarEq
    | Token.Operator OperatorSlashEq
    | Token.Operator OperatorPercentageEq
    | Token.Operator OperatorHatEq
    -> (
        match ParseExpr.parse_binop_assign ast with
        | Ok b -> Ok (Expr (b))
        | Error e -> Error e)
    | Token.Keyword KeywordAnd 
    | Token.Keyword KeywordOr
    -> (
        match ParseExpr.parse_binop_logical ast with
        | Ok b -> Ok (Expr (b))
        | Error e -> Error e)
    | Token.Keyword KeywordNot -> (
        match ParseExpr.parse_unary ast with
        | Ok u -> Ok (Expr (u))
        | Error e -> Error e)
    | Token.Keyword KeywordVar -> (
        match ParseExpr.parse_var ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordConst -> (
        match ParseExpr.parse_const ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Comment CommentOneLine -> (
        ParserUtil.next_token ast;
        Ok (Expr (ExprCommentOneLine)))
    | Token.Comment CommentMultiLine -> (
        if ast.current_location.s_line = ast.current_location.e_line then 
            (ParserUtil.next_token ast;
             ParseExpr.parse_end_line ast)
        else ParserUtil.next_token ast;
        Ok (Expr (ExprCommentMultiLine)))
    | Token.Comment CommentDoc s -> (
        if ast.current_location.s_line = ast.current_location.e_line then 
            (ParserUtil.next_token ast;
             ParseExpr.parse_end_line ast)
        else ParserUtil.next_token ast;
        Ok (Expr (ExprCommentDoc s)))
    | Token.Identifier s -> (
        match ParseExpr.parse_expr_identifier ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Separator SeparatorLeftParen -> (
        match ParseExpr.parse_tuple ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Separator SeparatorLeftHook -> (
        match ParseExpr.parse_array ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordFun -> (
        match ParseExpr.parse_fun_declare ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordImport -> (
        match ParseExpr.parse_import ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordShare -> (
        match ParseExpr.parse_share ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordAwait -> (
        match ParseExpr.parse_await ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordLambda -> (
        match ParseExpr.parse_anonymous_fun ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordClass -> (
        match ParseExpr.parse_class ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordPub -> (
        match ParseExpr.parse_pub ast with
        | Ok v -> Ok (Expr (v))
        | Error e -> Error e)
    | Token.Keyword KeywordIf -> (
        match ParseStmt.parse_stmt_if ast with
        | Ok v -> Ok (Stmt (v))
        | Error e -> Error e)
    | Token.Keyword KeywordSwitch -> (
        match ParseStmt.parse_stmt_switch ast with
        | Ok v -> Ok (Stmt (v))
        | Error e -> Error e)
    | Token.Keyword KeywordWhile -> (
        match ParseStmt.parse_stmt_while ast with
        | Ok v -> Ok (Stmt (v))
        | Error e -> Error e)
    | Token.Keyword KeywordFor -> (
        match ParseStmt.parse_stmt_for ast with
        | Ok v -> Ok (Stmt (v))
        | Error e -> Error e)
    | Token.Keyword KeywordLoop -> (
        match ParseStmt.parse_stmt_loop ast with
        | Ok v -> Ok (Stmt (v))
        | Error e -> Error e)
    | _ -> Error (ErrorIdUnexpectedAst)

let run_parser ast =
    let rec loop ast =
        if ast.pos < (CCVector.length (ast.stream.tok))-1 then
        match parser ast with
        | Error e -> print_error e ~line:ast.current_location.line ~col:ast.current_location.col ast.filename
        | Ok p -> (
            Printf.printf "%s\n" (ast_kind_to_str (p));
            push_ast (new_stream_ast) p;
            loop (ast)) in
    loop (ast)
