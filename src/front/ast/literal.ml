open Ast
open LilyFront.Token
open LilyFront.Error

let token_to_literal token =
  match token with
  | Keyword KeywordTrue -> Ok (LiteralBool (true))
  | Keyword KeywordFalse -> Ok (LiteralBool (false))
  | Literal (LiteralInt (v,_)) -> Ok (LiteralInt (v))
  | Literal (LiteralFloat (v,_)) -> Ok (LiteralFloat (v))
  | Literal (LiteralString s) -> Ok (LiteralString (s))
  | Literal (LiteralChar c) -> Ok (LiteralChar (c))
  | _ -> Error ErrorIdInvalidPrimaryType
