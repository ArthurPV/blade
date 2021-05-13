open LilyFront.Lexer
open LilyFront.Read

let run_tokenizer lex = 
    let t = CCVector.create () in
    let rec loop lex =
    if lex.info.pos < lex.read.length-1 then
        match tokenizer lex with
        | Error _ -> exit 1;
        | Ok tok -> (CCVector.push t tok;
                     LexerUtil.next_char lex;
                     loop (lex)) in
    loop (lex);
    t

let lexer_test filename =
    match GetFileContent.get_file_content (GetFileContent.read_lines filename) with
    | Ok s -> (let read = new_read filename s in 
               match read with
               | Ok r -> (let lexer_info = new_lexer_info in 
                          let lexer = new_lexer lexer_info r in
                          run_tokenizer lexer)
               | Error _ -> exit 1)
    | Error _ -> exit 1
