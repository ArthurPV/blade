type error_id = 
    | ErrorIdUnexpectedToken
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
    | ErrorIdUnexpectedIdentifier
    | ErrorIdUnexpectedExpr
    | ErrorIdUnexpectedAst

let error_id_to_str id =
    match id with
    | ErrorIdUnexpectedToken -> "unexpected token"
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
    | ErrorIdUnexpectedExpr -> "unexpected expression"
    | ErrorIdUnexpectedAst -> "unexpected AST"

let print_error err line col = 
    Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: %s, location: %d:%d\027[0m\n" (error_id_to_str err) line col;
    exit 1
