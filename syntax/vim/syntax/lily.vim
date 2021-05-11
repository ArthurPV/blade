" Vim syntax file
" Language: Blade
" Maintainer: ArthurPV

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "lily"

syn keyword bladeStorage explicit pub priv ref self virtual
syn keyword bladeStatement break next
syn keyword bladeConditional if elif else switch then and or not
syn keyword bladeRepeat while for loop
syn keyword bladeVariableKeyword const var new
syn keyword bladeVariableModifieurKeyword mutable

syn keyword bladeModuleDefault List Array Map Math Object Std Matrix Http Fs 

syn keyword bladeConstant nil undef
syn keyword bladeKeyword data fun end in of import class try catch throw finally type async await is module as share init macro

syn keyword bladeBoolean true false

syn keyword bladeType char string bool unit int float usize isize

syn match bladeType "\v<[iuf][1-9]\d*>"

syn match bladeOperator display "\V\[-+/*=^&?|!><%~]"

syn match bladeDecNumber display   "\v<\d%(_?\d)*%(\.\.@!)?%(\d%(_?\d)*)?%([eE][+-]?\d%(_?\d)*)?"
syn match bladeHexNumber display "\v<0x\x%(_?\x)*%(\.\.@!)?%(\x%(_?\x)*)?%([pP][+-]?\d%(_?\d)*)?"
syn match bladeOctNumber display "\v<0o\o%(_?\o)*"
syn match bladeBinNumber display "\v<0b[01]%(_?[01])*"

syn match bladeCharacterInvalid display contained /b\?'\zs[\n\r\t']\ze'/
syn match bladeCharacterInvalidUnicode display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
syn match bladeCharacter /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/ contains=bladeEscape,bladeEscapeError,bladeCharacterInvalid,bladeCharacterInvalidUnicode
syn match bladeCharacter /'\([^\\]\|\\\(.\|x\x\{2}\|u\x\{4}\|U\x\{6}\)\)'/ contains=bladeEscape,bladeEscapeUnicode,bladeEscapeError,bladeCharacterInvalid

syn keyword bladeCommentLineDocKeyword @end @doc @test

syn region bladeCommentLine start="\*\*" end="$" contains=bladeTodo,@Spell
syn region bladeCommentMultiLine start="(\*" end="\*)" contains=bladeTodo,@Spell
syn region bladeCommentLineDoc start="(\*\*" end="\*\*)" contains=bladeTodo,@Spell
syn region bladeMacro start="@" end="("
" syn region bladeCommentLineDocAt start="@" end="$" contains=bladeCommentLineDocKeyword,@Spell

" TODO: match only the first '\\' within the bladeMultilineString as bladeMultilineStringPrefix
syn match bladeMultilineStringPrefix display contained /c\?\\\\/
syn region bladeMultilineString start="c\?\\\\" end="$" contains=bladeMultilineStringPrefix

syn keyword bladeTodo contained TODO

syn match     bladeEscapeError   display contained /\\./
syn match     bladeEscape        display contained /\\\([nrt\\'"]\|x\x\{2}\)/
syn match     bladeEscapeUnicode display contained /\\\(u\x\{4}\|U\x\{6}\)/
syn region    bladeString      start=+c\?"+ skip=+\\\\\|\\"+ end=+"+ oneline contains=bladeEscape,bladeEscapeUnicode,bladeEscapeError,@Spell

hi def link bladeDecNumber bladeNumber
hi def link bladeHexNumber bladeNumber
hi def link bladeOctNumber bladeNumber
hi def link bladeBinNumber bladeNumber

hi def link bladeKeyword Keyword
hi def link bladeType Type
hi def link bladeMacro PreCondit
hi def link bladeCommentLine Comment
hi def link bladeCommentMultiLine Comment
hi def link bladeCommentLineDoc SpecialComment
hi def link bladeTodo Todo
hi def link bladeString String
hi def link bladeMultilineString String
hi def link bladeMultilineStringContent String
hi def link bladeMultilineStringPrefix Comment
hi def link bladeCharacterInvalid Error
hi def link bladeCharacterInvalidUnicode bladeCharacterInvalid
hi def link bladeCharacter Character
hi def link bladeEscape Special
hi def link bladeEscapeUnicode bladeEscape
hi def link bladeEscapeError Error
hi def link bladeBoolean Boolean
hi def link bladeConstant Constant
hi def link bladeNumber Number
hi def link bladeOperator Operator
hi def link bladeStorage StorageClass
hi def link bladeStatement Statement
hi def link bladeConditional Conditional
hi def link bladeRepeat Repeat
hi def link bladeModuleDefault PreProc
hi def link bladeVariableKeyword Keyword
hi def link bladeVariableModifieurKeyword StorageClass
hi def link bladeCommentLineDocKeyword PreCondit
" hi def link bladeCommentLineDocAt PreCondit
