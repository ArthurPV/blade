open Token

type 'a location = {
    line: int;
    col: int;
    s_line: int;
    s_col: int;
    e_line: int;
    e_col: int;
}

val new_location : 'a location

type 'a stream_token = {
    items: ('a token * 'a location) Stack.t
}

val new_stream_token : 'a stream_token

val push_token : 'a stream_token -> 'a token -> 'a location -> unit
