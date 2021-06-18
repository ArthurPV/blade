(*
add information to error msg
ex: ErrorIdExpectedToken "!"
*)
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

(*
the vectors are for accumulation of errors in lexer for the moment
*)
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

(*
default value for error type
*)
let new_error line_error = {
    id = CCVector.create ();
    line = CCVector.create ();
    col = CCVector.create ();
    s_line = CCVector.create ();
    s_col = CCVector.create ();
    e_line = CCVector.create ();
    e_col = CCVector.create ();
    count = 0;
}

(*
get string line of error
*)
let get_line_error error filename pos = 
    let ic = open_in filename in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let count = ref 0 in
    let rec loop acc =
        match try_read () with
        | Some s -> (
            if !count = CCVector.get error.s_line pos || !count <= CCVector.get error.e_line pos then
                (count := !count+1;
                loop (s :: acc))
            else 
                (count := !count+1;
                 loop (acc)))
        | None -> close_in ic; List.rev acc in
    loop []

(*
push differents informations for accumulation of errors
*)
let push_error error id ~line ~col ~s_line ~s_col ~e_line ~e_col = 
    CCVector.push error.id id;
    CCVector.push error.line line;
    CCVector.push error.col col;
    CCVector.push error.s_line s_line;
    CCVector.push error.s_col s_col; 
    CCVector.push error.e_line e_line;
    CCVector.push error.e_col e_col;
    error.count <- error.count + 1

(*
convert error_id to error message
*)
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

(*
print error and exit 1
*)
let print_error id ~line ~col filename = 
    Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename line col;
    Printf.printf "\027[1m\027[31m[lily:error]\027\027[0m\027[0m: %s\n" (error_id_to_str id);
    exit 1

(*
accumulation of errors
*)
let print_errors error filename = 
    for i = 0 to (CCVector.length error.id)-1 do
        Printf.printf "\027[1mFile \"%s\", location %d:%d\027[0m\n" filename (CCVector.get error.line i) (CCVector.get error.col i);
        Printf.printf "\027[1m\027[31m[lily:error]\027\027[0m\027[1m: %s\027[0m\n\n" (error_id_to_str (CCVector.get error.id i));
        Printf.printf "\027[1m%s\027[0m\n\n" (String.concat "" (get_line_error error filename i));
    done;
    if error.count > 0 then Printf.printf "\027[1m\027[31mError\027[0m\027[1m: Lily has emited %d errors\027[0m\n" (error.count)
    else ()
