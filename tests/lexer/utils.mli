open LilyFront.Lexer
open LilyFront.Token

val run_tokenizer : 'a lexer -> 'a token CCVector.vector

val lexer_test : string -> 'a token CCVector.vector
