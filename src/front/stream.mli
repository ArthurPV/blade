open Token
open CCVector

type 'a location = {
    line: int;
    col: int;
    s_line: int;
    s_col: int;
    e_line: int;
    e_col: int;
}

val new_location : int -> int -> int -> int -> int -> int -> 'a location

type 'a stream_token = {
    mutable tok: 'a token vector;
    mutable loc: 'a location vector;
}

val new_stream_token : 'a stream_token

val push_token : 'a stream_token -> 'a Token.token -> 'a location -> unit 
