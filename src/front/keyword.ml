open Token

(*
convert string (id) to token type
or else identifier
*)
let value_to_keyword value = 
    match value with
    | "explicit" -> Keyword KeywordExplicit
    | "pub" -> Keyword KeywordPub
    | "ref" -> Keyword KeywordRef
    | "self" -> Keyword KeywordSelf
    | "virtual" -> Keyword KeywordVirtual
    | "break" -> Keyword KeywordBreak
    | "next" -> Keyword KeywordNext
    | "if" -> Keyword KeywordIf
    | "elif" -> Keyword KeywordElif
    | "else" -> Keyword KeywordElse
    | "switch" -> Keyword KeywordSwitch
    | "then" -> Keyword KeywordThen
    | "and" -> Keyword KeywordAnd
    | "or" -> Keyword KeywordOr
    | "not" -> Keyword KeywordNot
    | "while" -> Keyword KeywordWhile
    | "for" -> Keyword KeywordFor
    | "loop" -> Keyword KeywordLoop
    | "const" -> Keyword KeywordConst
    | "var" -> Keyword KeywordVar
    | "new" -> Keyword KeywordNew
    | "nil" -> Keyword KeywordNil
    | "undef" -> Keyword KeywordUndef
    | "data" -> Keyword KeywordData
    | "fun" -> Keyword KeywordFun
    | "end" -> Keyword KeywordEnd
    | "in" -> Keyword KeywordIn
    | "import" -> Keyword KeywordImport
    | "class" -> Keyword KeywordClass
    | "try" -> Keyword KeywordTry
    | "catch" -> Keyword KeywordCatch
    | "throw" -> Keyword KeywordThrow
    | "finally" -> Keyword KeywordFinally
    | "type" -> Keyword KeywordType
    | "async" -> Keyword KeywordAsync
    | "await" -> Keyword KeywordAwait
    | "is" -> Keyword KeywordIs
    | "module" -> Keyword KeywordModule
    | "as" -> Keyword KeywordAs
    | "share" -> Keyword KeywordShare
    | "init" -> Keyword KeywordInit
    | "macro" -> Keyword KeywordMacro
    | "test" -> Keyword KeywordTest
    | "true" -> Keyword KeywordTrue
    | "false" -> Keyword KeywordFalse
    | "return" -> Keyword KeywordReturn
    | "char" -> Keyword KeywordChar
    | "i8" -> Keyword KeywordI8
    | "i16" -> Keyword KeywordI16
    | "i32" -> Keyword KeywordI32
    | "i64" -> Keyword KeywordI64
    | "i128" -> Keyword KeywordI128
    | "u8" -> Keyword KeywordU8
    | "u16" -> Keyword KeywordU16
    | "u32" -> Keyword KeywordU32
    | "u64" -> Keyword KeywordU64
    | "u128" -> Keyword KeywordU128
    | "string" -> Keyword KeywordString
    | "bool" -> Keyword KeywordBool
    | "unit" -> Keyword KeywordUnit
    | "usize" -> Keyword KeywordUsize
    | "isize" -> Keyword KeywordIsize
    | "lambda" -> Keyword KeywordLambda
    | _ -> Identifier value
