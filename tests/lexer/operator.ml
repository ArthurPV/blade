open OUnit2
open LilyFront.Token

(* + ++ += *)
let test_plus test_ctxt =
    let filename = "inputs/operators/plus.li" in
    let t = Utils.lexer_test filename in
    assert_equal (CCVector.get t 0) (Operator OperatorPlusEq);
    assert_equal (CCVector.get t 1) (Operator OperatorPlusPlus);
    assert_equal (CCVector.get t 2) (Operator OperatorPlus)

(* - -- -= *)
let test_minus test_ctxt =
    let filename = "inputs/operators/minus.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorMinusEq);
    assert_equal (CCVector.get t 1) (Operator OperatorMinusMinus);
    assert_equal (CCVector.get t 2) (Operator OperatorMinus)

(* *= * *)
let test_star test_ctxt =
    let filename = "inputs/operators/star.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorStarEq);
    assert_equal (CCVector.get t 1) (Operator OperatorStar)

(* /= / *)
let test_slash test_ctxt =
    let filename = "inputs/operators/slash.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorSlashEq);
    assert_equal (CCVector.get t 1) (Operator OperatorSlash)

(* %= % *)
let test_percentage test_ctxt =
    let filename = "inputs/operators/percentage.li" in
    let t = Utils.lexer_test filename in
    assert_equal (CCVector.get t 0) (Operator OperatorPercentageEq);
    assert_equal (CCVector.get t 1) (Operator OperatorPercentage)

(* ^= ^ *)
let test_hat test_ctxt =
    let filename = "inputs/operators/hat.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorHatEq);
    assert_equal (CCVector.get t 1) (Operator OperatorHat)

(* <= <> < *)
let test_left_shift test_ctxt = 
    let filename = "inputs/operators/left_shift.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorLeftShiftEq);
    assert_equal (CCVector.get t 1) (Operator OperatorLeftShiftRightShift);
    assert_equal (CCVector.get t 2) (Operator OperatorLeftShift)

(* >= > *)
let test_right_shift test_ctxt = 
    let filename = "inputs/operators/right_shift.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorRightShiftEq);
    assert_equal (CCVector.get t 1) (Operator OperatorRightShift)

(* == = ? *)
let test_other_operator test_ctxt = 
    let filename = "inputs/operators/other.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Operator OperatorEqEq);
    assert_equal (CCVector.get t 1) (Operator OperatorEq);
    assert_equal (CCVector.get t 2) (Operator OperatorInterogation)
