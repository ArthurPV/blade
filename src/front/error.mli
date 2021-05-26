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

type error = {
    mutable id: error_id array;
    mutable line: int array;
    mutable col: int array;
    mutable count: int;
}

val new_error : error

val push_error : error -> error_id -> line:int -> col:int -> unit

val error_id_to_str : error_id -> string

val print_error : error_id -> line:int -> col:int -> string -> unit

val print_errors : error -> string -> unit
