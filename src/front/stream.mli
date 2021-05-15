open Token

type location = {
    line: int;
    col: int;
    s_line: int;
    s_col: int;
    e_line: int;
    e_col: int;
}

val new_location : int -> int -> int -> int -> int -> int -> location

type stream_token = {
    mutable tok: token CCVector.vector;
    mutable loc: location CCVector.vector;
}

val new_stream_token : stream_token

val push_token : stream_token -> Token.token -> location -> unit 
