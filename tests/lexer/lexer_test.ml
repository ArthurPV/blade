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

let comparaison_operator_suite = 
    "comparaison_operator_suite">:::
        ["test_left_shift">:: Operator.test_left_shift;
         "test_right_shift">:: Operator.test_right_shift;
         "test_other_operator">:: Operator.test_other_operator;]

let keyword_suite =
    "keyword_suite">:::
        ["test_keywords">:: Keyword.test_keywords;]

let separator_suite = 
    "separator_suite">:::
        ["test_separators">:: Separator.test_separators;]

let _ =
    run_test_tt_main basic_operator_suite;
    run_test_tt_main special_operator_suite;
    run_test_tt_main comparaison_operator_suite;
    run_test_tt_main keyword_suite;
    run_test_tt_main separator_suite
