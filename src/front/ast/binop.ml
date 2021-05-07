open Ast
open Blade_front.Error
open Blade_front.Token

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
    | _ -> Error ErrorIdInvalidBinop
