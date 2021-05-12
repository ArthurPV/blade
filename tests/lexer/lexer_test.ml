open OUnit2

let operator_suite =
    "operator_suite">:::
        ["test_plus">:: Operator.test_plus;
         "test_minus">:: Operator.test_minus;
         "test_star">:: Operator.test_star;
         "test_slash">:: Operator.test_slash;
         "test_percentage">:: Operator.test_percentage]

let _ =
    run_test_tt_main operator_suite
