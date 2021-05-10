type separator = 
    | SeparatorDot
    | SeparatorDollar
    | SeparatorComma
    | SeparatorColon
    | SeparatorColonColon
    | SeparatorNewline
    | SeparatorVerticalBar
    | SeparatorArrow
    | SeparatorInverseArrow
    | SeparatorFatArrow
    | SeparatorAt
    | SeparatorLeftParen
    | SeparatorRightParen
    | SeparatorLeftBrace
    | SeparatorRightBrace
    | SeparatorLeftHook
    | SeparatorRightHook

type operator = 
    | OperatorPlus
    | OperatorMinus
    | OperatorStar
    | OperatorSlash
    | OperatorPercentage
    | OperatorHat
    | OperatorPlusPlus
    | OperatorMinusMinus
    | OperatorPlusEq
    | OperatorMinusEq
    | OperatorStarEq
    | OperatorSlashEq
    | OperatorPercentageEq
    | OperatorHatEq
    | OperatorEq
    | OperatorEqEq
    | OperatorEqDotDot
    | OperatorDotDot
    | OperatorDotDotEq
    | OperatorLeftShift
    | OperatorRightShift
    | OperatorLeftShiftEq
    | OperatorRightShiftEq
    | OperatorLeftShiftRightShift
    | OperatorInterogation

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
    | KeywordUsize
    | KeywordIsize

type int_format = 
    | Normal
    | Hexadecimal
    | Octal
    | Binary

type float_format = 
    | Normal
    | Scientific

type literal = 
    | LiteralInt of int * int_format
    | LiteralFloat of float * float_format
    | LiteralChar of char
    | LiteralString of string

type comment = 
    | CommentOneLine
    | CommentMultiLine
    | CommentDoc of string

type 'a token = 
    | Separator of separator
    | Operator of operator
    | Keyword of keyword
    | Identifier of string
    | Literal of literal
    | Comment of comment

val token_to_str : 'a token -> string 
