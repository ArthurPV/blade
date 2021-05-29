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

type error = {
    mutable id: error_id array;
    mutable line: int array;
    mutable col: int array;
    mutable count: int;
}

let new_error = {
    id = [||];
    line = [||];
    col = [||];
    count = 0;
}

let push_error error id ~line ~col = 
    error.id <- Stdlib.Array.append error.id [|id|];
    error.line <- Stdlib.Array.append error.line [|line|];
    error.col <- Stdlib.Array.append error.col [|col|];
    error.count <- error.count + 1

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
    | ErrorIdUnexpectedScope -> "unexpected scope"

let print_error id ~line ~col filename = 
    Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename line col;
    Printf.printf "\027[1m\027[31mError\027\027[0m\027[0m: %s\n" (error_id_to_str id);
    exit 1

let print_errors error filename = 
    for i = 0 to (Stdlib.Array.length error.id)-1 do
        Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename (Stdlib.Array.get error.line i) (Stdlib.Array.get error.col i);
        Printf.printf "\027[1m\027[31mError\027\027[0m\027[1m: %s\n\n" (error_id_to_str (Stdlib.Array.get error.id i));
    done;
    if error.count > 0 then Printf.printf "\027[1m\027[31mError\027[0m\027[1m: Lily has emited %d errors\027[0m\n" (error.count)
    else ()
