open LilyFront.Lexer
open LilyFront.Read

let test_tokenizer lex = 
    match tokenizer lex with
    | Error _ -> exit 1
    | Ok tok -> tok

let lexer_test filename =
    match GetFileContent.get_file_content (GetFileContent.read_lines filename) with
    | Ok s -> (let read = new_read filename s in 
               match read with
               | Ok r -> (let lexer_info = new_lexer_info in 
                          let lexer = new_lexer lexer_info r in
                          lexer)
               | Error _ -> exit 1)
    | Error _ -> exit 1

let test filename = test_tokenizer (lexer_test filename)
