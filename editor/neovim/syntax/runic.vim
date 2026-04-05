if exists("b:current_syntax")
  finish
endif

lua << EOF
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

parser_config.runic = {
    install_info = {
      -- path to the directory containing grammar.js and src/parser.c
      url = "/home/lumorsunil/repos/runic/editor/neovim/tree-sitter",
      files = { "src/parser.c" },  -- add "src/scanner.c" or ".cc" if you have one
      generate_requires_npm = false,
      requires_generate_from_grammar = false,
    },
    filetype = "runic",  -- internal name; can be "rn" if you prefer
}
EOF

" Basic Vim syntax fallback/highlighting for Runic.
"
" This complements tree-sitter instead of replacing it, and keeps core language
" keywords highlighted even when the parser is missing coverage.

" Keywords
syn keyword runicDeclKeyword const var fn pub
syn keyword runicTypeKeyword error enum union struct
syn keyword runicAsyncKeyword async await
syn keyword runicControlKeyword if else for while match return exit
syn keyword runicModuleKeyword import try catch and or orelse
syn keyword runicBoolean true false
syn keyword runicConstant null

" Builtins / command-like forms
syn keyword runicBuiltin echo cd

" Types commonly used in source/docs
syn keyword runicType String Int Float Bool Void Byte
syn keyword runicType Thread Execution

" Comments
syn match runicLineComment "#.*$" contains=runicTodo
syn region runicBlockComment start="/\*" end="\*/" contains=runicTodo
syn keyword runicTodo TODO FIXME XXX NOTE contained

" Strings and interpolation markers
syn region runicString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=runicInterp
syn region runicString start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=runicInterp
syn match runicInterp "\${"

" Numbers
syn match runicFloat "\<[0-9]\+\.[0-9]\+\>"
syn match runicNumber "\<[0-9]\+\>"

" Operators / punctuation
syn match runicAssignOperator "+=\|-=\|\*=\|/=\|%="
syn match runicCompareOperator "==\|!=\|>=\|<=\|=>\|->"
syn match runicRedirectOperator ">>&\|>&\|>>\|1>\|2>\|>"
syn match runicPipeOperator "||\||\|&&\|&\|!\|?\|\^"
syn match runicDelimiter "[.,:;()\[\]{}]"

" Highlight links
hi def link runicDeclKeyword Keyword
hi def link runicTypeKeyword Keyword
hi def link runicAsyncKeyword Keyword
hi def link runicControlKeyword Conditional
hi def link runicModuleKeyword Keyword
hi def link runicBoolean Boolean
hi def link runicConstant Constant
hi def link runicBuiltin Function
hi def link runicType Type
hi def link runicLineComment Comment
hi def link runicBlockComment Comment
hi def link runicTodo Todo
hi def link runicString String
hi def link runicInterp Special
hi def link runicFloat Float
hi def link runicNumber Number
hi def link runicAssignOperator Operator
hi def link runicCompareOperator Operator
hi def link runicRedirectOperator Operator
hi def link runicPipeOperator Operator
hi def link runicDelimiter Delimiter

let b:current_syntax = "runic"
