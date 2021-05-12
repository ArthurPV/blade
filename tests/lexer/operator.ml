open OUnit2
open LilyFront.Token
open LilyFront.Stream

(* + ++ += *)
let test_plus test_ctxt =
  let st = new_stream_token in
  Utils.test "./inputs/plus.li";
  assert_equal (CCVector.get st.tok 0) (Operator OperatorPlusEq);
  assert_equal (CCVector.get st.tok 1) (Operator OperatorPlusPlus);
  assert_equal (CCVector.get st.tok 2) (Operator OperatorPlus)

(* - -- -= *)
let test_minus test_ctxt =
  let st = new_stream_token in
  Utils.test "./inputs/minus.li";
  assert_equal (CCVector.get st.tok 0) (Operator OperatorMinusEq);
  assert_equal (CCVector.get st.tok 1) (Operator OperatorMinusMinus);
  assert_equal (CCVector.get st.tok 2) (Operator OperatorMinus)
