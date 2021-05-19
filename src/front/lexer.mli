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

type lexer = {
    info: lexer_info; 
    read: read;
}

val new_lexer_info : lexer_info

val new_lexer : lexer_info -> read -> lexer

module LexerUtil : sig
    val next_char : lexer -> unit
    val previous_char : lexer -> unit
    val start_token : lexer -> unit
    val end_token : lexer -> unit
    val get_next_char : lexer -> char
end

module RecognizeChar : sig 
    val is_digit : lexer -> bool
    val is_identifier : lexer -> bool
    val is_hex : lexer -> bool
    val is_bin : lexer -> bool
    val is_num : lexer -> bool
end

val get_escape : lexer -> (string, error_id) result

module ScanChar : sig 
    val scan_comment_one_line : lexer -> unit
    val scan_comment_multi_line : lexer -> (unit, error_id) result
    val scan_comment_doc : lexer -> (string, error_id) result
    val scan_identifier : lexer -> string
    val scan_char : lexer -> (char, error_id) result
    val scan_string : lexer -> (string, error_id) result
    val scan_hex : lexer -> (int, error_id) result
    val scan_oct : lexer -> (int, error_id) result
    val scan_bin : lexer -> (int, error_id) result
    val scan_num : lexer -> (token, error_id) result
end

val tokenizer : lexer -> (token, error_id) result

val run_tokenizer : lexer -> unit
