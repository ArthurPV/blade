type error_id = 
    | ErrorIdUnexpectedToken
    | ErrorIdInvalidCharLiteral
    | ErrorIdInvalidStringLiteral
    | ErrorIdInvalidEscape
    | ErrorIdInvalidHexadecimalLiteral
    | ErrorIdInvalidOctalLiteral

val error_id_to_str : error_id -> string
