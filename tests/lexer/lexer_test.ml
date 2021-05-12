open OUnit2
open Operator
open LilyFront.Token

let test_plus test_ctxt =
  match plus with
  | Ok p -> assert_equal (Operator (OperatorPlus)) p
  | Error _ -> exit 1

let test_plus_equal test_ctxt =
  match plus_equal with
  | Ok p -> assert_equal (Operator (OperatorPlusEq)) p
  | Error _ -> exit 1

let operator_suite =
  "operator_suite">:::
    ["test_plus">:: test_plus;
     "test_plus_equal">:: test_plus_equal]
;;

let _ =
  run_test_tt_main operator_suite
