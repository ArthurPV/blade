open OUnit2
open LilyFront.Token
open LilyFront.Stream

(* + ++ += *)
let test_plus test_ctxt =
    let st = new_stream_token in
    Utils.test "inputs/operators/plus.li";
    assert_equal (CCVector.get st.tok 0) (Operator OperatorPlusEq);
    assert_equal (CCVector.get st.tok 1) (Operator OperatorPlusPlus);
    assert_equal (CCVector.get st.tok 2) (Operator OperatorPlus)

(* - -- -= *)
let test_minus test_ctxt =
    let st = new_stream_token in
    Utils.test "inputs/operators/minus.li";
    assert_equal (CCVector.get st.tok 0) (Operator OperatorMinusEq);
    assert_equal (CCVector.get st.tok 1) (Operator OperatorMinusMinus);
    assert_equal (CCVector.get st.tok 2) (Operator OperatorMinus)

(* *= * *)
let test_star test_ctxt = 
    let st = new_stream_token in
    Utils.test "inputs/operators/star.li";
    assert_equal (CCVector.get st.tok 0) (Operator OperatorStarEq);
    assert_equal (CCVector.get st.tok 1) (Operator OperatorStar)

(* /= / *)
let test_slash test_ctxt = 
    let st = new_stream_token in
    Utils.test "inputs/operators/slash.li";
    assert_equal (CCVector.get st.tok 0) (Operator OperatorSlashEq);
    assert_equal (CCVector.get st.tok 1) (Operator OperatorSlash)

(* %= % *)
let test_percentage test_ctxt = 
    let st = new_stream_token in
    Utils.test "inputs/operators/percentage.li";
    assert_equal (CCVector.get st.tok 0) (Operator OperatorPercentageEq);
    assert_equal (CCVector.get st.tok 1) (Operator OperatorPercentage)
