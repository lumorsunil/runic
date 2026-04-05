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

let b:current_syntax = "runic"
