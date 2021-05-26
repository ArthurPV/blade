open OUnit2
open LilyFront.Token

let test_floats test_ctxt =
    let filename = "inputs/floats/floats.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (Stdlib.Array.get t 0) (Literal (LiteralFloat (3.333, Normal)));
    assert_equal (Stdlib.Array.get t 2) (Literal (LiteralFloat (3000.0, Scientific)))
