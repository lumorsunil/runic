lua << EOF
local ok, highlights = pcall(require, "runic_highlight.highlights")
if not ok then
  vim.schedule(function()
    vim.api.nvim_err_writeln("runic_highlight: " .. tostring(highlights))
  end)
  return
end

for _, def in ipairs(highlights.definitions) do
  if def.capture and def.group then
    vim.cmd(("highlight default link %s %s"):format(def.capture, def.group))
  end
end
EOF
