open OUnit2
open LilyFront.Token
open LilyFront.Stream

let test_plus test_ctxt =
  let st = new_stream_token in
  Operator.plus;
  assert_equal (CCVector.get st.tok 0) (Operator OperatorPlusEq);
  assert_equal (CCVector.get st.tok 1) (Operator OperatorPlusPlus);
  assert_equal (CCVector.get st.tok 2) (Operator OperatorPlus)

let operator_suite =
  "operator_suite">:::
    ["test_plus">:: test_plus]

let _ =
  run_test_tt_main operator_suite
