open LilyFront.Lexer
open LilyFront.Read

(* test + *)
let plus =
  let read = new_read "" "+ " in
  match read with
  | Ok r -> (let info = new_lexer_info in
             let lexer = new_lexer info r in
             tokenizer lexer)
  | Error _ -> exit 1

(* test += *)
let plus_equal =
  let read = new_read "" "+= " in
  match read with
  | Ok r -> (let info = new_lexer_info in
             let lexer = new_lexer info r in
             tokenizer lexer)
  | Error _ -> exit 1
