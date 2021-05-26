open OUnit2
open LilyFront.Token

let test_identifiers test_ctxt = 
    let filename = "inputs/identifier/identifier.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (Stdlib.Array.get t 0) (Identifier "_a");
    assert_equal (Stdlib.Array.get t 1) (Identifier "c4");
    assert_equal (Stdlib.Array.get t 2) (Identifier "_c4");
    assert_equal (Stdlib.Array.get t 3) (Identifier "c")

let test_failed_identifiers test_ctxt = 
    let filename = "inputs/identifier/failed.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (Stdlib.Array.get t 0) (Literal (LiteralInt (4, Normal)));
    assert_equal (Stdlib.Array.get t 1) (Identifier "c")
