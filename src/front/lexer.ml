open Error
open Keyword
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

module RecognizeChar = struct
    let is_digit lex = 
        (lex.read.c >= '0' && lex.read.c <= '9')
    let is_identifier lex = 
        (lex.read.c >= 'a' && lex.read.c <= 'z') || (lex.read.c >= 'A' && lex.read.c <= 'Z') || lex.read.c = '_' || is_digit lex

    let is_hex lex = 
        (lex.read.c >= 'a' && lex.read.c <= 'f') || (lex.read.c >= 'A' && lex.read.c <= 'F') || (lex.read.c >= '0' && lex.read.c <= '9')

    let is_bin lex = 
        (lex.read.c >= '0' && lex.read.c <= '1')

    let is_num lex = 
        if lex.info.pos < lex.read.length-1 then
            (is_digit lex || (lex.read.c = '.' && UtilLexer.get_next_char lex != '.') || ((lex.read.c = 'e' || lex.read.c = 'E') && (UtilLexer.get_next_char lex >= '0' && UtilLexer.get_next_char lex <= '9') || UtilLexer.get_next_char lex = '-' || UtilLexer.get_next_char lex = '+'))
        else false
end

let get_escape lex = 
    match lex.read.c with
    | '\\' -> (match UtilLexer.get_next_char lex with
               | 'n' -> (UtilLexer.next_char lex;
                         Ok "\\n")
               | 't' -> (UtilLexer.next_char lex;
                         Ok "\\t")
               | 'v' -> (UtilLexer.next_char lex;
                         Ok "\\v")
               | 'r' -> (UtilLexer.next_char lex;
                         Ok "\\r")
               | '\000' -> (UtilLexer.next_char lex;
                            Ok "\\0")
               | _ -> Error ErrorIdInvalidEscape)
    | _ -> Ok (String.make 1 lex.read.c)

module ScanChar = struct
    let rec scan_comment_one_line lex = 
        if lex.read.c != '\n' then
            (UtilLexer.next_char lex;
            scan_comment_one_line (lex))
        else
            UtilLexer.previous_char lex

    let scan_comment_multi_line lex = 
        let rec loop lex = 
            if (lex.read.c != '*' || UtilLexer.get_next_char lex != ')') then 
                (UtilLexer.next_char lex;
                 loop (lex)) in
        loop (lex);

        if UtilLexer.get_next_char lex != ')' then Error (ErrorIdInvalidStringLiteral)
        else (UtilLexer.next_char lex;
              Ok ())

    let scan_comment_doc lex = 
        let value = ref [] in 
        UtilLexer.next_char lex;
        UtilLexer.next_char lex;
        UtilLexer.next_char lex;
        let rec loop lex = 
            if lex.read.c != '*' || UtilLexer.get_next_char lex != '*' || lex.read.content.[lex.info.pos+2] != ')' then 
                (value := !value @ [String.make 1 lex.read.c];
                 UtilLexer.next_char lex;
                 loop (lex)) in 
        loop (lex);

         if lex.read.content.[lex.info.pos+2] != ')' then Error ErrorIdInvalidStringLiteral
         else (UtilLexer.next_char lex;
               UtilLexer.next_char lex;
               Ok (String.concat "" !value))

    let scan_identifier lex = 
        let value = ref [] in
        let rec loop lex =
        if RecognizeChar.is_identifier lex then 
            (value := !value @ [String.make 1 lex.read.c];
             UtilLexer.next_char lex;
             loop (lex)) in 
        loop (lex);
        UtilLexer.previous_char lex;
        String.concat "" !value

    let scan_char lex = 
        UtilLexer.next_char lex;
        if lex.read.c != '\'' then 
            (UtilLexer.next_char lex;
             if lex.read.c != '\'' then 
                 Error (ErrorIdInvalidCharLiteral)
             else Ok lex.read.content.[lex.info.pos-1])
        else Error (ErrorIdInvalidCharLiteral)

    let scan_string lex = 
        UtilLexer.next_char lex;
        let value = ref [] in
        let rec loop lex = 
            if lex.read.c != '\"' && lex.info.pos < lex.read.length-1 then 
                (match get_escape lex with
                 | Error _ -> ()
                 | Ok s -> (value := !value @ [s];
                            UtilLexer.next_char lex;
                            loop (lex))) in
        loop (lex);
        if lex.read.c != '\"' then
            (if lex.info.pos != lex.read.length-1 then Error ErrorIdInvalidStringLiteral
             else Error ErrorIdInvalidEscape)
        else Ok (String.concat "" !value)

    let scan_hex lex = 
        let value = ref [] in
        value := !value @ [String.make 1 lex.read.c];
        UtilLexer.next_char lex; (* 0 *)
        value := !value @ [String.make 1 lex.read.c];
        UtilLexer.next_char lex; (* x *)

        let rec loop lex = 
            if RecognizeChar.is_hex lex then 
                (value := !value @ [String.make 1 lex.read.c];
                 UtilLexer.next_char lex;
                 loop (lex)) in 
        loop (lex);
        UtilLexer.previous_char lex;

        match String.concat "" !value with
        | "0x" -> Error ErrorIdInvalidStringLiteral
        | _ -> Ok (int_of_string (String.concat "" !value))

    let scan_oct lex = 
        let value = ref [] in
        value := !value @ [String.make 1 lex.read.c];
        UtilLexer.next_char lex; (* 0 *)
        value := !value @ [String.make 1 lex.read.c];
        UtilLexer.next_char lex; (* o *)

        let rec loop lex = 
            if RecognizeChar.is_digit lex then
                (value := !value @ [String.make 1 lex.read.c];
                 UtilLexer.next_char lex;
                 loop (lex)) in 
        loop (lex);
        UtilLexer.previous_char lex;

        match String.concat "" !value with
        | "0o" -> Error ErrorIdInvalidOctalLiteral
        | _ -> Ok (int_of_string (String.concat "" !value))

    let scan_bin lex = 
        let value = ref [] in
        value := !value @ [String.make 1 lex.read.c];
        UtilLexer.next_char lex; (* 0 *)
        value := !value @ [String.make 1 lex.read.c];
        UtilLexer.next_char lex; (* b *)

        let rec loop lex = 
            if RecognizeChar.is_bin lex then 
                (value := !value @ [String.make 1 lex.read.c];
                 UtilLexer.next_char lex;
                 loop (lex)) in 
        loop (lex);
        UtilLexer.previous_char lex;

        match String.concat "" !value with
        | "0b" -> Error ErrorIdInvalidBinaryLiteral
        | _ -> Ok (int_of_string (String.concat "" !value))

    let scan_num lex = 
        let is_float = ref false in
        let is_sct = ref false in
        let value = ref [] in 
        
        let rec loop lex = 
            if RecognizeChar.is_num lex then
                (if lex.read.c = '.' then is_float := true;
                 if lex.read.c = 'e' || lex.read.c = 'E' then
                     (is_sct := true; 
                      if UtilLexer.get_next_char lex = '-' || UtilLexer.get_next_char lex = '+' then 
                          (value := !value @ [String.make 1 lex.read.c];
                           UtilLexer.next_char lex;
                           value := !value @ [String.make 1 lex.read.c];
                           UtilLexer.next_char lex));
                 value := !value @ [String.make 1 lex.read.c];
                 UtilLexer.next_char lex;
                 loop (lex)) in 
        loop (lex); 
        UtilLexer.previous_char lex;

        if !is_float = true then Ok (Literal (LiteralFloat ((float_of_string (String.concat "" !value)), Normal)))
        else if !is_sct = true then Ok (Literal (LiteralFloat ((float_of_string (String.concat "" !value)), Scientific)))
        else Ok (Literal (LiteralInt ((int_of_string (String.concat "" !value)), Normal)))
end

let tokenizer lex = 
    UtilLexer.start_token lex;

    let rec loop lex = 
        if lex.read.c = ' ' || lex.read.c = '\t' then
            (UtilLexer.next_char lex;
             loop (lex)) in 
    loop (lex);

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

    | '(' -> (match UtilLexer.get_next_char lex with
              | '*' -> (match lex.read.content.[lex.info.pos+2] with
                        | '*' -> (match ScanChar.scan_comment_doc lex with
                                  | Ok s -> Ok (Comment (CommentDoc s))
                                  | Error e -> Error e)
                        | _ -> (match ScanChar.scan_comment_multi_line lex with
                                | Ok _ -> Ok (Comment (CommentMultiLine))
                                | Error e -> Error e))
              | _ -> Ok (Separator (SeparatorLeftParen)))

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

    | '\'' -> (match ScanChar.scan_char lex with
               | Ok c -> Ok (Literal (LiteralChar (c)))
               | Error e -> Error e)

    | '\"' -> (match ScanChar.scan_string lex with
               | Ok t -> Ok (Literal (LiteralString (t)))
               | Error e -> Error e)

    | '0' -> (match UtilLexer.get_next_char lex with
              | 'x' -> (match ScanChar.scan_hex lex with
                        | Ok i -> Ok (Literal (LiteralInt (i, Hexadecimal)))
                        | Error e -> Error e)
              | 'o' -> (match ScanChar.scan_oct lex with
                        | Ok i -> Ok (Literal (LiteralInt (i, Octal)))
                        | Error e -> Error e)
              | 'b' -> (match ScanChar.scan_bin lex with
                        | Ok i -> Ok (Literal (LiteralInt (i, Binary)))
                        | Error e -> Error e)
              | '.' | '0' .. '9' | 'e' | 'E' -> (match ScanChar.scan_num lex with
                        | Ok i -> Ok i
                        | Error _ -> Error ErrorIdInvalidNumLiteral) 
              | _ -> Ok (Literal (LiteralInt (0, Normal))))

    | '0' .. '9' -> (match ScanChar.scan_num lex with 
                     | Ok i -> Ok i
                     | Error _ -> Error ErrorIdInvalidNumLiteral)

    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (match value_to_keyword (ScanChar.scan_identifier lex) with
                     | Ok t -> Ok t
                     | Error e -> Ok e)

    | _ -> Error ErrorIdUnexpectedToken

let rec run_tokenizer lex = 
    if lex.info.pos < lex.read.length-1 then
        match tokenizer lex with
        | Ok t -> (Printf.printf "%d:%d -> %s\n" lex.info.line lex.info.col (token_to_str t);
                   UtilLexer.next_char lex;
                   run_tokenizer (lex))
        | Error _ -> Printf.printf "error\n";
