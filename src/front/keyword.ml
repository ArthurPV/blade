open Token

let value_to_keyword value = 
    match value with
    | "pub" -> Keyword KeywordPub
    | _ -> Identifier value
