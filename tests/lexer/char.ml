open OUnit2
open LilyFront.Token

let test_char test_ctxt = 
    let filename = "inputs/chars/chars.li" in
    let t = Utils.lexer_test filename in
    assert_equal (Stdlib.Array.get t 0) (Literal (LiteralChar 's'))
