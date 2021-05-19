open LilyAst.Ast
open LilyAst.Parser
open LilyFront.Lexer
open LilyFront.Read
open LilyFront.Stream
open LilyAst.Stream

let run filename =
    match GetFileContent.get_file_content (GetFileContent.read_lines filename) with
    | Ok s -> (let read = new_read filename s in 
               match read with
               | Ok r -> (let lexer_info = new_lexer_info in
                          let lexer = new_lexer lexer_info r in 
                          run_tokenizer lexer;
                          let stream_token = new_stream_token in
                          let ast = new_ast (stream_token) lexer in
                          let stream_ast = new_stream_ast in
                          Printf.printf "%d\n" (CCVector.length ast.stream.tok);
                          run_parser ast;
                          Printf.printf "%d\n" (CCVector.length stream_ast.kind))
               | Error e -> Printf.printf "%s\n" e)
    | Error e -> Printf.printf "%s" e
