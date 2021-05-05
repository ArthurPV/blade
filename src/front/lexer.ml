open Read

type lexer_info = {
    mutable line: int;
    mutable col: int;
    mutable s_line: int;
    mutable s_col: int;
    mutable e_line: int;
    mutable e_col: int;
    mutable pos: int;
}

type 'a lexer = {
    info: lexer_info; 
    read: 'a read;
}

let new_lexer_info = {
    line = 1;
    col = 1;
    s_line = 1;
    s_col = 1;
    e_line = 1;
    e_col = 1;
    pos = 0;
}

let new_lexer info read = {
    info = info;
    read = read;
}

module UtilLexer = struct 
    let next_token lexer = 
        if lexer.read.c = '\n' then
            (lexer.info.pos <- lexer.info.pos + 1;
             lexer.info.col <- 1;
             lexer.info.line <- lexer.info.line + 1;
             lexer.read.c <- lexer.read.content.[lexer.info.pos];
             ())
        else
            (lexer.info.pos <- lexer.info.pos + 1;
             lexer.info.col <- lexer.info.col + 1;
             lexer.read.c <- lexer.read.content.[lexer.info.pos];
             ())
end
