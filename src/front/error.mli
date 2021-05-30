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
    | ErrorIdUnexpectedScope
    | ErrorIdUnexpectedImportValue

type error = {
    mutable id: error_id array;
    mutable line: int array;
    mutable col: int array;
    mutable s_line: int array;
    mutable s_col: int array;
    mutable e_line: int array;
    mutable e_col: int array;
    mutable count: int;
    mutable line_error: string;
}

val new_error : string -> error

val get_line_error : error -> string -> int -> string list

val push_error : 
    error -> 
    error_id -> 
    line:int -> 
    col:int -> 
    s_line:int ->
    s_col:int ->
    e_line:int ->
    e_col: int ->
    unit

val error_id_to_str : error_id -> string

val print_error : 
    error_id -> 
    line:int -> 
    col:int -> 
    string -> 
    unit

val print_errors : error -> string -> unit