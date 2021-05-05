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

val new_lexer_info : lexer_info

val new_lexer : lexer_info -> 'a read -> 'a lexer

module UtilLexer : sig
    val next_token : 'a lexer -> unit
end
