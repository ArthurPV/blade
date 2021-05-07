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
    tok: 'a token Stack.t;
    loc: 'a location Stack.t;
}

let new_stream_token = {
    tok = Stack.create ();
    loc = Stack.create ();
}

let push_token st tok loc = 
    Stack.push tok st.tok;
    Stack.push loc st.loc
