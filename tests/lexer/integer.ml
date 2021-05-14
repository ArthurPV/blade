open OUnit2
open LilyFront.Token

let test_integer test_ctxt = 
    let filename = "inputs/integers/integers.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Literal (LiteralInt (44, Normal)));
    assert_equal (CCVector.get t 2) (Literal (LiteralInt (255, Hexadecimal)));
    assert_equal (CCVector.get t 4) (Literal (LiteralInt (18, Octal)));
    assert_equal (CCVector.get t 6) (Literal (LiteralInt (5, Binary)))
