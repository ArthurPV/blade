open Ast
open LilyFront.Error

let token_to_type ast = 
    match ast.current_token with
    | Keyword KeywordChar -> Ok LilyTypeChar
    | Keyword KeywordI8 -> Ok LilyTypeI8
    | Keyword KeywordI16 -> Ok LilyTypeI16
    | Keyword KeywordI32 -> Ok LilyTypeI32
    | Keyword KeywordI64 -> Ok LilyTypeI64
    | Keyword KeywordI128 -> Ok LilyTypeI128
    | Keyword KeywordU8 -> Ok LilyTypeU8
    | Keyword KeywordU16 -> Ok LilyTypeU16
    | Keyword KeywordU32 -> Ok LilyTypeU32
    | Keyword KeywordU64 -> Ok LilyTypeU64
    | Keyword KeywordU128 -> Ok LilyTypeU128
    | Keyword KeywordString -> Ok LilyTypeString
    | Keyword KeywordBool -> Ok LilyTypeBool
    | Keyword KeywordUnit -> Ok LilyTypeUnit
    | Identifier s -> Ok (LilyTypeUserDefinedType s)
    | _ -> Error ErrorIdUnexpectedType
