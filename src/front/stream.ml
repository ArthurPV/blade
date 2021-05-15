open Token

type location = {
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

type stream_token = {
    mutable tok: token CCVector.vector;
    mutable loc: location CCVector.vector;
}

let new_stream_token = {
    tok = CCVector.create ();
    loc = CCVector.create ();
}

let push_token st tok loc = 
    CCVector.push st.tok tok;
    CCVector.push st.loc loc;
