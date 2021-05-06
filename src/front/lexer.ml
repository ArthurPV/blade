open Error
open Read
open Token

type lexer_info = {
    mutable line: int;
    mutable col: int;
    mutable s_line: int;
    mutable s_col: int;
    mutable e_line: int;
    mutable e_col: int;
    mutable pos: int;
}

type 'a lexer = {
    info: lexer_info; 
    read: 'a read;
}

let new_lexer_info = {
    line = 1;
    col = 1;
    s_line = 1;
    s_col = 1;
    e_line = 1;
    e_col = 1;
    pos = 0;
}

let new_lexer info read = {
    info = info;
    read = read;
}

module UtilLexer = struct 
    let next_char lex = 
        if lex.read.c = '\n' then
            (lex.info.pos <- lex.info.pos + 1;
             lex.info.col <- 1;
             lex.info.line <- lex.info.line + 1;
             lex.read.c <- lex.read.content.[lex.info.pos];
             ())
        else
            (lex.info.pos <- lex.info.pos + 1;
             lex.info.col <- lex.info.col + 1;
             lex.read.c <- lex.read.content.[lex.info.pos];
             ())

    let previous_char lex =
        lex.info.pos <- lex.info.pos - 1;
        lex.info.col <- lex.info.col - 1;
        lex.read.c <- lex.read.content.[lex.info.pos];
        ()

    let start_token lex =
        lex.info.s_line <- lex.info.line;
        lex.info.s_col <- lex.info.col;
        ()

    let end_token lex =
        lex.info.e_line <- lex.info.line;
        lex.info.e_col <- lex.info.col;
        ()

    let get_next_char lex = lex.read.content.[lex.info.pos+1]
end

module ScanChar = struct
    let rec scan_comment_one_line lex = 
        if lex.read.c != '\n' then
            (UtilLexer.next_char lex;
            scan_comment_one_line (lex))
end

let tokenizer lex = 
    UtilLexer.start_token lex;
    match lex.read.c with
    | '$' -> Ok (Separator SeparatorDollar)
    | ',' -> Ok (Separator SeparatorComma)
    | ':' -> (match UtilLexer.get_next_char lex with
              | ':' -> (UtilLexer.next_char lex;
                        Ok (Separator SeparatorColonColon))
              | _ -> Ok (Separator SeparatorColon))
    | '\n' -> Ok (Separator SeparatorNewline)
    | '|' -> Ok (Separator SeparatorVerticalBar)
    | '@' -> Ok (Separator SeparatorAt)
    | '(' -> Ok (Separator SeparatorLeftParen)
    | ')' -> Ok (Separator SeparatorRightParen)
    | '{' -> Ok (Separator SeparatorLeftBrace)
    | '}' -> Ok (Separator SeparatorRightBrace)
    | '[' -> Ok (Separator SeparatorLeftHook)
    | ']' -> Ok (Separator SeparatorRightHook)
    | '+' -> (match UtilLexer.get_next_char lex with
              | '+' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorPlusPlus))
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorPlusEq))
              | _ -> Ok (Operator OperatorPlus))
    | '-' -> (match UtilLexer.get_next_char lex with
              | '-' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorMinusMinus))
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorMinusEq))
              | '>' -> (UtilLexer.next_char lex;
                        Ok (Separator SeparatorArrow))
              | _ -> Ok (Operator OperatorMinus))
    | '*' -> (match UtilLexer.get_next_char lex with
              | '*' -> (ScanChar.scan_comment_one_line lex;
                        Ok (Comment CommentOneLine))
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorStarEq))
              | _ -> Ok (Operator OperatorStar))
    | '/' -> (match UtilLexer.get_next_char lex with
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorSlashEq))
              | _ -> Ok (Operator OperatorSlash))
    | '%' -> (match UtilLexer.get_next_char lex with
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorPercentageEq))
              | _ -> Ok (Operator OperatorPercentage))
    | '^' -> (match UtilLexer.get_next_char lex with
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorHatEq))
              | _ -> Ok (Operator OperatorHat))
    | '=' -> (match UtilLexer.get_next_char lex with
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorEqEq))
              | '.' -> (match lex.read.content.[lex.info.pos+2] with
                        | '.' -> (UtilLexer.next_char lex;
                                  UtilLexer.next_char lex;
                                  Ok (Operator OperatorEqDotDot))
                        | _ -> Error ErrorIdUnexpectedToken)
              | _ -> Ok (Operator OperatorEq))
    | '<' -> (match UtilLexer.get_next_char lex with
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorLeftShiftEq))
              | '-' -> (UtilLexer.next_char lex;
                        Ok (Separator SeparatorInverseArrow))
              | _ -> Ok (Operator OperatorLeftShift))
    | '>' -> (match UtilLexer.get_next_char lex with
              | '=' -> (UtilLexer.next_char lex;
                        Ok (Operator OperatorRightShiftEq))
              | _ -> Ok (Operator OperatorRightShift))
    | '.' -> (match UtilLexer.get_next_char lex with
              | '.' -> (match lex.read.content.[lex.info.pos+2] with
                        | '=' -> (UtilLexer.next_char lex;
                                  UtilLexer.next_char lex;
                                  Ok (Operator OperatorDotDotEq))
                        | _ -> (UtilLexer.next_char lex;
                                Ok (Operator OperatorDotDot)))
              | _ -> Ok (Separator SeparatorDot))
    | '?' -> Ok (Operator OperatorInterogation)
    | _ -> Error ErrorIdUnexpectedToken

let rec run_tokenizer lex = 
    if lex.info.pos < lex.read.length-1 then
        match tokenizer lex with
        | Ok t -> (Printf.printf "%s\n" (token_to_str t);
                   UtilLexer.next_char lex;
                   run_tokenizer (lex))
        | Error _ -> Printf.printf "error\n";
