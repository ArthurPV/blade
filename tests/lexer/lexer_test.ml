open OUnit2


let operator_suite =
  "operator_suite">:::
      ["test_plus">:: Operator.test_plus;
       "test_minus">:: Operator.test_minus]

let _ =
  run_test_tt_main operator_suite
