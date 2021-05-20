type error_id = 
    | ErrorIdUnexpectedToken of string
    | ErrorIdExpectedToken of string
    | ErrorIdMissToken
    | ErrorIdInvalidCharLiteral
    | ErrorIdInvalidStringLiteral
    | ErrorIdInvalidEscape
    | ErrorIdInvalidHexadecimalLiteral
    | ErrorIdInvalidOctalLiteral
    | ErrorIdInvalidBinaryLiteral
    | ErrorIdInvalidNumLiteral
    | ErrorIdInvalidBinop
    | ErrorIdInvalidUnary
    | ErrorIdInvalidPrimaryType
    | ErrorIdInvalidValue
    | ErrorIdUnexpectedType
    | ErrorIdMissIdentifier
    | ErrorIdUnexpectedIdentifier
    | ErrorIdUnexpectedExpr
    | ErrorIdUnexpectedAst
    | ErrorIdSyntaxError

let error_id_to_str id =
    match id with
    | ErrorIdUnexpectedToken s -> Printf.sprintf "unexpected token: \'%s\'" s
    | ErrorIdExpectedToken s -> Printf.sprintf "expected token: \"%s\"" s
    | ErrorIdMissToken -> "miss token"
    | ErrorIdInvalidCharLiteral -> "invalid char literal"
    | ErrorIdInvalidStringLiteral -> "invalid string literal"
    | ErrorIdInvalidEscape -> "invalid escape"
    | ErrorIdInvalidHexadecimalLiteral -> "invalid hexadecimal literal"
    | ErrorIdInvalidOctalLiteral -> "invalid octal literal"
    | ErrorIdInvalidBinaryLiteral -> "invalid binary literal"
    | ErrorIdInvalidNumLiteral -> "invalid num literal"
    | ErrorIdInvalidBinop -> "invalid binop"
    | ErrorIdInvalidUnary -> "invalid unary"
    | ErrorIdInvalidPrimaryType -> "invalid primary type"
    | ErrorIdInvalidValue -> "invalid value"
    | ErrorIdUnexpectedType -> "unexpected type"
    | ErrorIdUnexpectedIdentifier -> "unexpected identifier"
    | ErrorIdMissIdentifier -> "miss identifier"
    | ErrorIdUnexpectedExpr -> "unexpected expression"
    | ErrorIdUnexpectedAst -> "unexpected AST"
    | ErrorIdSyntaxError -> "syntax error"

let print_error err line col filename = 
    Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename line col;
    Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: %s\n" (error_id_to_str err);
    exit 1
