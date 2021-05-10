open Ast
open LilyFront.Error
open LilyFront.Token

let token_to_binop tok = 
    match tok with
    | Operator OperatorPlus -> Ok BinopAdd
    | Operator OperatorMinus -> Ok BinopSub
    | Operator OperatorStar -> Ok BinopMul
    | Operator OperatorSlash -> Ok BinopDiv
    | Operator OperatorPercentage -> Ok BinopMod
    | Operator OperatorHat -> Ok BinopPow
    | Operator OperatorPlusPlus -> Ok BinopMerge
    | Operator OperatorMinusMinus -> Ok BinopReplace
    | Operator OperatorPlusEq -> Ok BinopAddAssign
    | Operator OperatorMinusEq -> Ok BinopSubAssign
    | Operator OperatorStarEq -> Ok BinopMulAssign
    | Operator OperatorSlashEq -> Ok BinopDivAssign
    | Operator OperatorPercentageEq -> Ok BinopModAssign
    | Operator OperatorHatEq -> Ok BinopPowAssign
    | Operator OperatorEq -> Ok BinopAssign
    | Operator OperatorLeftShiftRightShift -> Ok BinopNotEq
    | Operator OperatorEqEq -> Ok BinopEq
    | Operator OperatorEqDotDot -> Ok BinopEqInterval
    | Operator OperatorDotDot -> Ok BinopInterval
    | Operator OperatorDotDotEq -> Ok BinopIntervalEq
    | Operator OperatorLeftShift -> Ok BinopLess
    | Operator OperatorLeftShiftEq -> Ok BinopLessEq 
    | Operator OperatorRightShift -> Ok BinopGreater
    | Operator OperatorRightShiftEq -> Ok BinopGreaterEq 
    | Operator OperatorInterogation -> Ok BinopCondition
    | Keyword KeywordAnd -> Ok BinopAnd
    | Keyword KeywordOr -> Ok BinopOr
    | _ -> Error ErrorIdInvalidBinop
