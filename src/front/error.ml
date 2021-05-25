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
    mutable id: error_id CCVector.vector;
    mutable line: int CCVector.vector;
    mutable col: int CCVector.vector;
    mutable count: int;
}

let new_error = {
    id = CCVector.create ();
    line = CCVector.create ();
    col = CCVector.create ();
    count = 0;
}

let push_error error id ~line ~col = 
    CCVector.push error.id id;
    CCVector.push error.line line;
    CCVector.push error.col col;
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

let print_error id ~line ~col filename = 
    Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename line col;
    Printf.printf "\027[1m\027[31mError\027\027[0m\027[0m: %s\n" (error_id_to_str id);
    exit 1

let print_errors error filename = 
    for i = 0 to (CCVector.length error.id)-1 do
        Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename (CCVector.get error.line i) (CCVector.get error.col i);
        Printf.printf "\027[1m\027[31mError\027\027[0m\027[0m: %s\n\n" (error_id_to_str (CCVector.get error.id i));
    done;
    if error.count > 0 then Printf.printf "\027[1m\027[31mLily has emited %d errors\027[0m\n" (error.count)
    else ()

