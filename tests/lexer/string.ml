open OUnit2
open LilyFront.Token

let test_string test_ctxt = 
    let filename = "inputs/strings/strings.li" in
    let t = Utils.lexer_test filename in
    assert_equal (Stdlib.Array.get t 0) (Literal (LiteralString "hello"))
