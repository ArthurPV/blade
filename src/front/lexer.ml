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
    let next_char lex = 
        if lex.read.c = '\n' then
            (lex.info.pos <- lex.info.pos + 1;
             lex.info.col <- 1;
             lex.info.line <- lex.info.line + 1;
             lex.read.c <- lex.read.content.[lex.info.pos];
             ())
        else
            (lex.info.pos <- lex.info.pos + 1;
             lex.info.col <- lex.info.col + 1;
             lex.read.c <- lex.read.content.[lex.info.pos];
             ())

    let previous_char lex =
        lex.info.pos <- lex.info.pos - 1;
        lex.info.col <- lex.info.col - 1;
        lex.read.c <- lex.read.content.[lex.info.pos];
        ()

    let start_token lex =
        lex.info.s_line <- lex.info.line;
        lex.info.s_col <- lex.info.col;
        ()

    let end_token lex =
        lex.info.e_line <- lex.info.line;
        lex.info.e_col <- lex.info.col;
        ()
end

let rec lexer lex = 
    if lex.info.pos < lex.read.length then 
        (Printf.printf "hello";
         UtilLexer.next_char lex;
         lexer (lex))
    else
        (Printf.printf "bye")
