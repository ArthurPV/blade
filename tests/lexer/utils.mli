open LilyFront.Lexer
open LilyFront.Token

val run_tokenizer : lexer -> token CCVector.vector

val lexer_test : string -> token CCVector.vector
