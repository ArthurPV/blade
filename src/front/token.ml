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
    | Keyword KeywordPriv -> "priv"
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
    | Keyword KeywordOf -> "of"
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
    | Keyword KeywordTrue -> "true"
    | Keyword KeywordFalse -> "false"
    | Keyword KeywordChar -> "Char"
    | Keyword KeywordI8 -> "I8"
    | Keyword KeywordI16 -> "I16"
    | Keyword KeywordI32 -> "I32"
    | Keyword KeywordI64 -> "I64"
    | Keyword KeywordI128 -> "I128"
    | Keyword KeywordU8 -> "U8"
    | Keyword KeywordU16 -> "U16"
    | Keyword KeywordU32 -> "U32"
    | Keyword KeywordU64 -> "U64"
    | Keyword KeywordU128 -> "U128"
    | Keyword KeywordString -> "String"
    | Keyword KeywordBool -> "Bool"
    | Keyword KeywordUnit -> "Unit"
    | Keyword KeywordUsize -> "Usize"
    | Keyword KeywordIsize -> "Isize"
    | Identifier s -> Printf.sprintf "Identifier -> %s" s
    | Literal LiteralInt (l,_) -> Printf.sprintf "Integer -> %d" l
    | Literal LiteralFloat (l,_) -> Printf.sprintf "Float -> %f" l
    | Literal LiteralChar l -> Printf.sprintf "Char -> %c" l
    | Literal LiteralString l -> Printf.sprintf "String -> %s" l
    | Comment CommentOneLine -> "**"
    | Comment CommentMultiLine -> "(* *)"
    | Comment CommentDoc s -> Printf.sprintf "(** %s **)" s       
