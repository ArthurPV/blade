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

let new_error line_error = {
    id = [||];
    line = [||];
    col = [||];
    s_line = [||];
    s_col = [||];
    e_line = [||];
    e_col = [||];
    count = 0;
    line_error = line_error;
}

let get_line_error error filename pos = 
    let ic = open_in filename in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let count = ref 0 in
    let rec loop acc =
        match try_read () with
        | Some s -> (
            if !count = Stdlib.Array.get error.s_line pos && !count <= Stdlib.Array.get error.e_line pos then
                (count := !count+1;
                loop (s :: acc))
            else 
                (count := !count+1;
                 loop (acc)))
        | None -> close_in ic; List.rev acc in
    loop []

let push_error error id ~line ~col ~s_line ~s_col ~e_line ~e_col = 
    error.id <- Stdlib.Array.append error.id [|id|];
    error.line <- Stdlib.Array.append error.line [|line|];
    error.col <- Stdlib.Array.append error.col [|col|];
    error.s_line <- Stdlib.Array.append error.s_line [|s_line|];
    error.s_col <- Stdlib.Array.append error.s_col [|s_col|]; 
    error.e_line <- Stdlib.Array.append error.e_line [|e_line|];
    error.e_col <- Stdlib.Array.append error.e_col [|e_col|];
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
    | ErrorIdUnexpectedImportValue -> "unexpected import value"

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
