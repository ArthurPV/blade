open OUnit2
open LilyFront.Token

let test_keyword test_ctxt = 
    let filename = "inputs/keywords/keywords.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Keyword KeywordExplicit);
    assert_equal (CCVector.get t 1) (Keyword KeywordPub);
    assert_equal (CCVector.get t 2) (Keyword KeywordPriv);
    assert_equal (CCVector.get t 3) (Keyword KeywordRef);
    assert_equal (CCVector.get t 4) (Keyword KeywordSelf);
    assert_equal (CCVector.get t 5) (Keyword KeywordVirtual);
    assert_equal (CCVector.get t 6) (Keyword KeywordBreak);
    assert_equal (CCVector.get t 7) (Keyword KeywordNext);
    assert_equal (CCVector.get t 8) (Keyword KeywordIf);
    assert_equal (CCVector.get t 9) (Keyword KeywordElif);
    assert_equal (CCVector.get t 10) (Keyword KeywordElse);
    assert_equal (CCVector.get t 11) (Keyword KeywordSwitch);
    assert_equal (CCVector.get t 12) (Keyword KeywordThen);
    assert_equal (CCVector.get t 13) (Keyword KeywordAnd);
    assert_equal (CCVector.get t 14) (Keyword KeywordOr);
    assert_equal (CCVector.get t 15) (Keyword KeywordNot);
    assert_equal (CCVector.get t 16) (Keyword KeywordWhile);
    assert_equal (CCVector.get t 17) (Keyword KeywordFor);
    assert_equal (CCVector.get t 18) (Keyword KeywordLoop);
    assert_equal (CCVector.get t 19) (Keyword KeywordConst);
    assert_equal (CCVector.get t 20) (Keyword KeywordVar);
    assert_equal (CCVector.get t 21) (Keyword KeywordNew);
    assert_equal (CCVector.get t 22) (Keyword KeywordNil);
    assert_equal (CCVector.get t 23) (Keyword KeywordUndef);
    assert_equal (CCVector.get t 24) (Keyword KeywordData);
    assert_equal (CCVector.get t 25) (Keyword KeywordFun);
    assert_equal (CCVector.get t 26) (Keyword KeywordEnd);
    assert_equal (CCVector.get t 27) (Keyword KeywordIn);
    assert_equal (CCVector.get t 28) (Keyword KeywordOf);
    assert_equal (CCVector.get t 29) (Keyword KeywordImport);
    assert_equal (CCVector.get t 30) (Keyword KeywordClass);
    assert_equal (CCVector.get t 31) (Keyword KeywordTry);
    assert_equal (CCVector.get t 32) (Keyword KeywordCatch);
    assert_equal (CCVector.get t 33) (Keyword KeywordThrow);
    assert_equal (CCVector.get t 34) (Keyword KeywordFinally);
    assert_equal (CCVector.get t 35) (Keyword KeywordType);
    assert_equal (CCVector.get t 36) (Keyword KeywordAsync);
    assert_equal (CCVector.get t 37) (Keyword KeywordAwait);
    assert_equal (CCVector.get t 38) (Keyword KeywordIs);
    assert_equal (CCVector.get t 39) (Keyword KeywordModule);
    assert_equal (CCVector.get t 40) (Keyword KeywordAs);
    assert_equal (CCVector.get t 41) (Keyword KeywordShare);
    assert_equal (CCVector.get t 42) (Keyword KeywordInit);
    assert_equal (CCVector.get t 43) (Keyword KeywordMacro);
    assert_equal (CCVector.get t 44) (Keyword KeywordTrue);
    assert_equal (CCVector.get t 45) (Keyword KeywordFalse);
    assert_equal (CCVector.get t 46) (Keyword KeywordChar);
    assert_equal (CCVector.get t 47) (Keyword KeywordI8);
    assert_equal (CCVector.get t 48) (Keyword KeywordI16);
    assert_equal (CCVector.get t 49) (Keyword KeywordI32);
    assert_equal (CCVector.get t 50) (Keyword KeywordI64);
    assert_equal (CCVector.get t 51) (Keyword KeywordI128);
    assert_equal (CCVector.get t 52) (Keyword KeywordU8);
    assert_equal (CCVector.get t 53) (Keyword KeywordU16);
    assert_equal (CCVector.get t 54) (Keyword KeywordU32);
    assert_equal (CCVector.get t 55) (Keyword KeywordU64);
    assert_equal (CCVector.get t 56) (Keyword KeywordU128);
    assert_equal (CCVector.get t 57) (Keyword KeywordString);
    assert_equal (CCVector.get t 58) (Keyword KeywordBool);
    assert_equal (CCVector.get t 59) (Keyword KeywordUnit);
    assert_equal (CCVector.get t 60) (Keyword KeywordUsize);
    assert_equal (CCVector.get t 61) (Keyword KeywordIsize);
