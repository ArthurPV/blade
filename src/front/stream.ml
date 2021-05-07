open Token

type 'a location = {
    line: int;
    col: int;
    s_line: int;
    s_col: int;
    e_line: int;
    e_col: int;
}

let new_location line col s_line s_col e_line e_col = {
    line = line;
    col = col;
    s_line = s_line;
    s_col = s_col;
    e_line = e_line;
    e_col = e_col;
}

type 'a stream_token = {
    items: ('a token * 'a location) Stack.t
}

let new_stream_token = {
    items = Stack.create ();
}

let push_token st tok loc = 
    Stack.push (tok, loc) st.items
