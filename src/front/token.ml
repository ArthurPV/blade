type separator = 
    | SeparatorDot
    | SeparatorDotDotDot
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
    | SeparatorHashtag

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
    | KeywordTest
    | KeywordTrue
    | KeywordFalse
    | KeywordReturn
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
    | KeywordLambda

type int_format = 
    | Normal
    | Hexadecimal
    | Octal
    | Binary

type float_format = 
    | Normal
    | Scientific

type literal = 
    | LiteralInt of string * int_format
    | LiteralFloat of string * float_format
    | LiteralChar of char
    | LiteralString of string

type comment = 
    | CommentOneLine
    | CommentMultiLine
    | CommentDoc of string

type token = 
    | Separator of separator
    | Operator of operator
    | Keyword of keyword
    | Identifier of string
    | Literal of literal
    | Comment of comment

let token_to_str tok = 
    match tok with
    | Separator SeparatorDot -> "."
    | Separator SeparatorDotDotDot -> "..."
    | Separator SeparatorDollar -> "$"
    | Separator SeparatorComma -> ","
    | Separator SeparatorColon -> ":"
    | Separator SeparatorColonColon -> "::"
    | Separator SeparatorNewline -> "Newline"
    | Separator SeparatorVerticalBar -> "|"
    | Separator SeparatorArrow -> "->"
    | Separator SeparatorInverseArrow -> "<-"
    | Separator SeparatorFatArrow -> "=>"
    | Separator SeparatorAt -> "@"
    | Separator SeparatorLeftParen -> "("
    | Separator SeparatorRightParen -> ")"
    | Separator SeparatorLeftBrace -> "{"
    | Separator SeparatorRightBrace -> "}"
    | Separator SeparatorLeftHook -> "["
    | Separator SeparatorRightHook -> "]"
    | Separator SeparatorHashtag -> "#"
    | Operator OperatorPlus -> "+"
    | Operator OperatorMinus-> "-"
    | Operator OperatorStar -> "*"
    | Operator OperatorSlash -> "/"
    | Operator OperatorPercentage -> "%"
    | Operator OperatorHat -> "^"
    | Operator OperatorPlusPlus -> "++"
    | Operator OperatorMinusMinus -> "--"
    | Operator OperatorPlusEq -> "+="
    | Operator OperatorMinusEq -> "-="
    | Operator OperatorStarEq -> "*="
    | Operator OperatorSlashEq -> "/="
    | Operator OperatorPercentageEq -> "%="
    | Operator OperatorHatEq -> "^="
    | Operator OperatorEq -> "="
    | Operator OperatorEqEq -> "=="
    | Operator OperatorEqDotDot -> "=.."
    | Operator OperatorDotDot -> ".."
    | Operator OperatorDotDotEq -> "..="
    | Operator OperatorLeftShift -> "<"
    | Operator OperatorRightShift -> ">"
    | Operator OperatorLeftShiftEq -> "<="
    | Operator OperatorRightShiftEq -> ">="
    | Operator OperatorLeftShiftRightShift -> "<>"
    | Operator OperatorInterogation -> "?"
    | Keyword KeywordExplicit -> "explicit"
    | Keyword KeywordPub -> "pub"
    | Keyword KeywordRef -> "ref"
    | Keyword KeywordSelf -> "self"
    | Keyword KeywordVirtual -> "virtual"
    | Keyword KeywordBreak -> "break"
    | Keyword KeywordNext -> "next"
    | Keyword KeywordIf -> "if"
    | Keyword KeywordElif -> "elif"
    | Keyword KeywordElse -> "else"
    | Keyword KeywordSwitch -> "switch"
    | Keyword KeywordThen -> "then"
    | Keyword KeywordAnd -> "and"
    | Keyword KeywordOr -> "or"
    | Keyword KeywordNot -> "not"
    | Keyword KeywordWhile -> "while"
    | Keyword KeywordFor -> "for"
    | Keyword KeywordLoop -> "loop"
    | Keyword KeywordConst -> "const"
    | Keyword KeywordVar -> "var"
    | Keyword KeywordNew -> "new"
    | Keyword KeywordNil -> "nil"
    | Keyword KeywordUndef -> "undef"
    | Keyword KeywordData -> "data"
    | Keyword KeywordFun -> "fun"
    | Keyword KeywordEnd -> "end"
    | Keyword KeywordIn -> "in"
    | Keyword KeywordImport -> "import"
    | Keyword KeywordClass -> "class"
    | Keyword KeywordTry -> "try"
    | Keyword KeywordCatch -> "catch"
    | Keyword KeywordThrow -> "throw"
    | Keyword KeywordFinally -> "finally"
    | Keyword KeywordType -> "type"
    | Keyword KeywordAsync -> "async"
    | Keyword KeywordAwait -> "await"
    | Keyword KeywordIs -> "is"
    | Keyword KeywordModule -> "module"
    | Keyword KeywordAs -> "as"
    | Keyword KeywordShare -> "share"
    | Keyword KeywordInit -> "init"
    | Keyword KeywordMacro -> "macro"
    | Keyword KeywordTest -> "test"
    | Keyword KeywordTrue -> "true"
    | Keyword KeywordFalse -> "false"
    | Keyword KeywordReturn -> "return"
    | Keyword KeywordChar -> "char"
    | Keyword KeywordI8 -> "i8"
    | Keyword KeywordI16 -> "i16"
    | Keyword KeywordI32 -> "i32"
    | Keyword KeywordI64 -> "i64"
    | Keyword KeywordI128 -> "i128"
    | Keyword KeywordU8 -> "u8"
    | Keyword KeywordU16 -> "u16"
    | Keyword KeywordU32 -> "u32"
    | Keyword KeywordU64 -> "u64"
    | Keyword KeywordU128 -> "u128"
    | Keyword KeywordString -> "string"
    | Keyword KeywordBool -> "bool"
    | Keyword KeywordUnit -> "unit"
    | Keyword KeywordUsize -> "usize"
    | Keyword KeywordIsize -> "isize"
    | Keyword KeywordLambda -> "lambda"
    | Identifier s -> Printf.sprintf "Identifier -> %s" s
    | Literal LiteralInt (l,_) -> Printf.sprintf "Integer -> %s" l
    | Literal LiteralFloat (l,_) -> Printf.sprintf "Float -> %s" l
    | Literal LiteralChar l -> Printf.sprintf "Char -> %c" l
    | Literal LiteralString l -> Printf.sprintf "String -> %s" l
    | Comment CommentOneLine -> "**"
    | Comment CommentMultiLine -> "(* *)"
    | Comment CommentDoc s -> Printf.sprintf "(** %s **)" s       
