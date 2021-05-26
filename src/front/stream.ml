open Token

type location = {
    line: int;
    col: int;
    s_line: int;
    s_col: int;
    e_line: int;
    e_col: int;
}

let new_location ~line ~col ~s_line ~s_col ~e_line ~e_col = {
    line = line;
    col = col;
    s_line = s_line;
    s_col = s_col;
    e_line = e_line;
    e_col = e_col;
}

type stream_token = {
    mutable tok: token array;
    mutable loc: location array;
}

let new_stream_token = {
    tok = [||];
    loc = [||];
}

let push_token st tok loc = 
    st.tok <- Stdlib.Array.append st.tok [|tok|];
    st.loc <- Stdlib.Array.append st.loc [|loc|];
