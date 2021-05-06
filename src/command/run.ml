open Blade_front.Lexer
open Blade_front.Read

let run filename =
    match GetFileContent.get_file_content (GetFileContent.read_lines filename) with
    | Ok s -> (let read = new_read filename s in 
               match read with
               | Ok r -> (let lexer_info = new_lexer_info in
                          let lexer = new_lexer lexer_info r in
                          run_tokenizer lexer)
               | Error e -> Printf.printf "%s" e)
    | Error e -> Printf.printf "%s" e
