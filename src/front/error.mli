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
    mutable id: error_id CCVector.vector;
    mutable line: int CCVector.vector;
    mutable col: int CCVector.vector;
    mutable s_line: int CCVector.vector;
    mutable s_col: int CCVector.vector;
    mutable e_line: int CCVector.vector;
    mutable e_col: int CCVector.vector;
    mutable count: int;
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
