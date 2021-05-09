open Kwhale_ast.Ast
open Kwhale_front.Lexer
open Kwhale_front.Read
open Kwhale_front.Stream

let run filename =
    match GetFileContent.get_file_content (GetFileContent.read_lines filename) with
    | Ok s -> (let read = new_read filename s in 
               match read with
               | Ok r -> (let lexer_info = new_lexer_info in
                          let lexer = new_lexer lexer_info r in 
                          let stream_token_location = new_location lexer.info.line lexer.info.col lexer.info.s_line lexer.info.e_line lexer.info.s_col lexer.info.e_col in
                          run_tokenizer lexer stream_token_location;
                          let stream_token = new_stream_token in
                          let ast = new_ast (stream_token) in 
                          Printf.printf "%d" (CCVector.length ast.stream.tok))
               | Error e -> Printf.printf "%s" e)
    | Error e -> Printf.printf "%s" e
