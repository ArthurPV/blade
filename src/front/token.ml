type keyword = 
    | KeywordExplicit
    | KeywordPub
    | KeywordPriv
    | KeywordRef
    | KeywordSelf
    | KeywordVirtual
    | KeywordBreak
    | KeywordNext
    | KeywordIf
    | KeywordElif
    | KeywordElse
    | KeywordSwitch
    | KeywordThen
    | KeywordAnd
    | KeywordOr
    | KeywordNot
    | KeywordWhile
    | KeywordFor
    | KeywordLoop
    | KeywordConst
    | KeywordVar
    | KeywordNew
    | KeywordNil
    | KeywordUndef
    | KeywordData
    | KeywordFun
    | KeywordEnd
    | KeywordIn
    | KeywordOf
    | KeywordImport
    | KeywordClass
    | KeywordTry
    | KeywordCatch
    | KeywordThrow
    | KeywordFinally
    | KeywordType
    | KeywordAsync
    | KeywordAwait
    | KeywordIs
    | KeywordModule
    | KeywordAs
    | KeywordShare
    | KeywordInit
    | KeywordMacro
    | KeywordTrue
    | KeywordFalse
    | KeywordChar
    | KeywordI8
    | KeywordI16
    | KeywordI32
    | KeywordI64
    | KeywordI128
    | KeywordU8
    | KeywordU16
    | KeywordU32
    | KeywordU64
    | KeywordU128
    | KeywordString
    | KeywordBool
    | KeywordUnit

type token_kind = 
    Keyword of keyword
