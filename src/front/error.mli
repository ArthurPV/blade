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

val error_id_to_str : error_id -> string

val print_error : error_id -> int -> int -> unit
