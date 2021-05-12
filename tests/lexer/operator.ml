open LilyFront.Lexer
open LilyFront.Read
open LilyFront.Stream

(* test += ++ + *)
let plus =
    match GetFileContent.get_file_content (GetFileContent.read_lines "./input/plus.li") with
    | Ok s -> (let read = new_read "./input/plus.li" s in 
               match read with
               | Ok r -> (let lexer_info = new_lexer_info in
                          let lexer = new_lexer lexer_info r in 
                          let stream_token_location = new_location lexer.info.line lexer.info.col lexer.info.s_line lexer.info.e_line lexer.info.s_col lexer.info.e_col in
                          run_tokenizer lexer stream_token_location;)
               | Error _ -> exit 1)
    | Error _ -> exit 1;
