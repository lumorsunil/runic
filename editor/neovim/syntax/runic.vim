if exists("b:current_syntax")
  finish
endif

lua << EOF
local ok, syntax = pcall(require, "runic_highlight.syntax")
if ok then
  syntax.setup()
else
  vim.schedule(function()
    vim.api.nvim_err_writeln("runic_highlight: " .. tostring(syntax))
  end)
end
EOF

let b:current_syntax = "runic"
