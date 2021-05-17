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

let comment_suite = 
    "comment_suite">:::
        ["test_comments">:: Comment.test_comments;]

let identifier_suite = 
    "identifier_suite">:::
        ["test_identifiers">:: Identifier.test_identifiers;
         "test_failed_identifiers">:: Identifier.test_failed_identifiers;]

let integer_suite = 
    "integer_suite">:::
        ["test_integer">:: Integer.test_integer;]

let float_suite = 
    "float_suite">:::
        ["test_floats">:: Float.test_floats;]

let char_suite = 
    "char_suite">:::
        ["test_char">:: Char.test_char;]

let string_suite = 
    "string_suite">:::
        ["test_string">:: String.test_string;]

let _ =
    run_test_tt_main basic_operator_suite;
    run_test_tt_main special_operator_suite;
    run_test_tt_main comparaison_operator_suite;
    run_test_tt_main keyword_suite;
    run_test_tt_main separator_suite;
    run_test_tt_main comment_suite;
    run_test_tt_main identifier_suite;
    run_test_tt_main integer_suite;
    run_test_tt_main float_suite;
    run_test_tt_main char_suite;
    run_test_tt_main string_suite
