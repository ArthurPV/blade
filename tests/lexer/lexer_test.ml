open OUnit2

let basic_operator_suite =
    "basic_operator_suite">:::
        ["test_plus">:: Operator.test_plus;
         "test_minus">:: Operator.test_minus;
         "test_star">:: Operator.test_star;
         "test_slash">:: Operator.test_slash;]

let special_operator_suite = 
    "special_operator_suite">:::
        ["test_percentage">:: Operator.test_percentage;
         "test_hat">:: Operator.test_hat;]

let _ =
    run_test_tt_main basic_operator_suite;
    run_test_tt_main special_operator_suite
