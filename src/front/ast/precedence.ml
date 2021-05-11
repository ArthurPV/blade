open LilyFront.Token

(* priority of operator
1 = priority more
10 = priority less
*)

let get_precedence tok = 
    match tok with
    | Separator SeparatorLeftParen 
    | Separator SeparatorRightParen 
    -> Some 1
    | Keyword KeywordNot  
    -> Some 2
    | Operator OperatorStar 
    | Operator OperatorSlash 
    | Operator OperatorPercentage 
    | Operator OperatorHat 
    -> Some 3
    | Operator OperatorLeftShift 
    | Operator OperatorLeftShiftEq 
    | Operator OperatorRightShift 
    | Operator OperatorRightShiftEq 
    -> Some 4
    | Operator OperatorEqEq 
    | Operator OperatorLeftShiftRightShift 
    -> Some 5
    | Keyword KeywordAnd -> Some 6
    | Keyword KeywordOr -> Some 7
    | Operator OperatorInterogation -> Some 8
    | Operator OperatorPlusEq 
    | Operator OperatorMinusEq 
    | Operator OperatorStarEq 
    | Operator OperatorSlashEq 
    | Operator OperatorPercentageEq 
    | Operator OperatorHatEq 
    -> Some 9
    | _ -> None
