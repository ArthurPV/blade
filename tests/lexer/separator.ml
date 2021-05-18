open OUnit2
open LilyFront.Token

let test_separators test_ctxt = 
    let filename = "inputs/separators/separators.li" in 
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Separator SeparatorDot);
    assert_equal (CCVector.get t 1) (Separator SeparatorDollar);
    assert_equal (CCVector.get t 2) (Separator SeparatorComma);
    assert_equal (CCVector.get t 3) (Separator SeparatorColon);
    assert_equal (CCVector.get t 4) (Separator SeparatorColonColon);
    assert_equal (CCVector.get t 5) (Separator SeparatorNewline);
    assert_equal (CCVector.get t 6) (Separator SeparatorVerticalBar);
    assert_equal (CCVector.get t 7) (Separator SeparatorArrow);
    assert_equal (CCVector.get t 8) (Separator SeparatorInverseArrow);
    assert_equal (CCVector.get t 9) (Separator SeparatorFatArrow);
    assert_equal (CCVector.get t 10) (Separator SeparatorAt);
    assert_equal (CCVector.get t 11) (Separator SeparatorLeftParen);
    assert_equal (CCVector.get t 12) (Separator SeparatorRightParen);
    assert_equal (CCVector.get t 13) (Separator SeparatorLeftBrace);
    assert_equal (CCVector.get t 14) (Separator SeparatorRightBrace);
    assert_equal (CCVector.get t 15) (Separator SeparatorLeftHook);
    assert_equal (CCVector.get t 16) (Separator SeparatorRightHook);
    assert_equal (CCVector.get t 17) (Separator SeparatorHashtag)
