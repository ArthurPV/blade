open Token

type 'a location = {
    line: int;
    col: int;
    s_line: int;
    s_col: int;
    e_line: int;
    e_col: int;
}

let new_location = {
    line = 1;
    col = 1;
    s_line = 1;
    s_col = 1;
    e_line = 1;
    e_col = 1;
}

type 'a stream_token = {
    items: ('a token * 'a location) Stack.t
}

let new_stream_token = {
    items = Stack.create ();
}

let push_token st tok loc = 
    Stack.push (tok, loc) st.items
