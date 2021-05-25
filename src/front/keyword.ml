open Token

let value_to_keyword value = 
    match value with
    | "explicit" -> Ok (Keyword KeywordExplicit)
    | "pub" -> Ok (Keyword KeywordPub)
    | "ref" -> Ok (Keyword KeywordRef)
    | "self" -> Ok (Keyword KeywordSelf)
    | "virtual" -> Ok (Keyword KeywordVirtual)
    | "break" -> Ok (Keyword KeywordBreak)
    | "next" -> Ok (Keyword KeywordNext)
    | "if" -> Ok (Keyword KeywordIf)
    | "elif" -> Ok (Keyword KeywordElif)
    | "else" -> Ok (Keyword KeywordElse)
    | "switch" -> Ok (Keyword KeywordSwitch)
    | "then" -> Ok (Keyword KeywordThen)
    | "and" -> Ok (Keyword KeywordAnd)
    | "or" -> Ok (Keyword KeywordOr)
    | "not" -> Ok (Keyword KeywordNot)
    | "while" -> Ok (Keyword KeywordWhile)
    | "for" -> Ok (Keyword KeywordFor)
    | "loop" -> Ok (Keyword KeywordLoop)
    | "const" -> Ok (Keyword KeywordConst)
    | "var" -> Ok (Keyword KeywordVar)
    | "new" -> Ok (Keyword KeywordNew)
    | "nil" -> Ok (Keyword KeywordNil)
    | "undef" -> Ok (Keyword KeywordUndef)
    | "data" -> Ok (Keyword KeywordData)
    | "fun" -> Ok (Keyword KeywordFun)
    | "end" -> Ok (Keyword KeywordEnd)
    | "in" -> Ok (Keyword KeywordIn)
    | "of" -> Ok (Keyword KeywordOf)
    | "import" -> Ok (Keyword KeywordImport)
    | "class" -> Ok (Keyword KeywordClass)
    | "try" -> Ok (Keyword KeywordTry)
    | "catch" -> Ok (Keyword KeywordCatch)
    | "throw" -> Ok (Keyword KeywordThrow)
    | "finally" -> Ok (Keyword KeywordFinally)
    | "type" -> Ok (Keyword KeywordType)
    | "async" -> Ok (Keyword KeywordAsync)
    | "await" -> Ok (Keyword KeywordAwait)
    | "is" -> Ok (Keyword KeywordIs)
    | "module" -> Ok (Keyword KeywordModule)
    | "as" -> Ok (Keyword KeywordAs)
    | "share" -> Ok (Keyword KeywordShare)
    | "init" -> Ok (Keyword KeywordInit)
    | "macro" -> Ok (Keyword KeywordMacro)
    | "test" -> Ok (Keyword KeywordTest)
    | "true" -> Ok (Keyword KeywordTrue)
    | "false" -> Ok (Keyword KeywordFalse)
    | "return" -> Ok (Keyword KeywordReturn)
    | "char" -> Ok (Keyword KeywordChar)
    | "i8" -> Ok (Keyword KeywordI8)
    | "i16" -> Ok (Keyword KeywordI16)
    | "i32" -> Ok (Keyword KeywordI32)
    | "i64" -> Ok (Keyword KeywordI64)
    | "i128" -> Ok (Keyword KeywordI128)
    | "u8" -> Ok (Keyword KeywordU8)
    | "u16" -> Ok (Keyword KeywordU16)
    | "u32" -> Ok (Keyword KeywordU32)
    | "u64" -> Ok (Keyword KeywordU64)
    | "u128" -> Ok (Keyword KeywordU128)
    | "string" -> Ok (Keyword KeywordString)
    | "bool" -> Ok (Keyword KeywordBool)
    | "unit" -> Ok (Keyword KeywordUnit)
    | "usize" -> Ok (Keyword KeywordUsize)
    | "isize" -> Ok (Keyword KeywordIsize)
    | "lambda" -> Ok (Keyword KeywordLambda)
    | _ -> Error (Identifier value)
