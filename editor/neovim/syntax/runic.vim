if exists("b:current_syntax")
  finish
endif

<<<<<<< Updated upstream
=======
lua << EOF
local ok, parser_config = pcall(require, "nvim-treesitter.parsers")
if ok and parser_config.get_parser_configs then
  parser_config.runic = {
      install_info = {
        url = "/home/jadu/codespace/runic/editor/neovim/tree-sitter",
        files = { "src/parser.c" },
        generate_requires_npm = false,
        requires_generate_from_grammar = false,
      },
      filetype = "runic",
  }
end
EOF

>>>>>>> Stashed changes
let b:current_syntax = "runic"

" Basic syntax highlighting for Runic
" Keywords
syn keyword runicKeyword fn const var if else for while return try catch match import error enum union
syn keyword runicBoolean true false null
syn keyword runicBuiltin echo cd

" Types
syn keyword runicType String Int Float Bool Void

" Comments
syn match runicComment "#.*$" contains=runicTodo
syn keyword runicTodo TODO FIXME XXX NOTE contained

" Strings
syn region runicString start="\"" end="\""
syn region runicString start="'" end="'"

" Numbers
syn match runicNumber "\<\d\+\(\.\d\+\)\?\>"

" Operators
syn match runicOperator "[|&!<>]=\?"
syn match runicArrow "->"
syn match runicFatArrow "=>"

" Pipeline
syn match runicPipeline "|"

" Highlight groups
hi def link runicKeyword     Keyword
hi def link runicBoolean    Boolean
hi def link runicBuiltin    Function
hi def link runicType       Type
hi def link runicComment    Comment
hi def link runicTodo       Todo
hi def link runicString     String
hi def link runicNumber     Number
hi def link runicOperator   Operator
hi def link runicArrow      Special
hi def link runicFatArrow   Special
hi def link runicPipeline   Special
