open OUnit2
open LilyFront.Lexer
open LilyFront.Token

(* + ++ += *)
let test_plus test_ctxt =
    let filename = "inputs/operators/plus.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorPlusEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorPlusPlus);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorPlus)

(* - -- -= *)
let test_minus test_ctxt =
    let filename = "inputs/operators/minus.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorMinusEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorMinusMinus);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorMinus)

(* *= * *)
let test_star test_ctxt =
    let filename = "inputs/operators/star.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorStarEq);
    LexerUtil.next_char lexer;
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorStar)

(* /= / *)
let test_slash test_ctxt =
    let filename = "inputs/operators/slash.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorSlashEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorSlash)

(* %= % *)
let test_percentage test_ctxt =
    let filename = "inputs/operators/percentage.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorPercentageEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorPercentage)

(* ^= ^ *)
let test_hat test_ctxt =
    let filename = "inputs/operators/hat.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorHatEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorHat)

(* <= < *)
let test_left_shift test_ctxt = 
    let filename = "inputs/operators/left_shift.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorLeftShiftEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorLeftShift)

(* >= > *)
let test_right_shift test_ctxt = 
    let filename = "inputs/operators/right_shift.li" in
    let lexer = Utils.lexer_test filename in
    assert_equal (Utils.test filename) (Operator OperatorRightShiftEq);
    LexerUtil.next_char lexer;
    assert_equal (Utils.test filename) (Operator OperatorRightShift)
