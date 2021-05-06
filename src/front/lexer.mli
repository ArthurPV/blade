open Error
open Read
open Token

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
    val next_char : 'a lexer -> unit
    val previous_char : 'a lexer -> unit
    val start_token : 'a lexer -> unit
    val end_token : 'a lexer -> unit
    val get_next_char : 'a lexer -> char
end

module RecognizeChar : sig 
    val is_identifier : 'a lexer -> bool
    val is_hex : 'a lexer -> bool
    val is_bin : 'a lexer -> bool
end

module ScanChar : sig 
    val scan_comment_one_line : 'a lexer -> unit
    val scan_identifier : 'a lexer -> string
    val scan_char : 'a lexer -> (char, error_id) result
    val scan_string : 'a lexer -> (string, error_id) result
    val scan_hex : 'a lexer -> (int, error_id) result
end

val tokenizer : 'a lexer -> ('a token, error_id) result

val run_tokenizer : 'a lexer -> unit
