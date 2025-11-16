if vim.g.loaded_runic_highlight then
  return
end
vim.g.loaded_runic_highlight = true

local ok, runic = pcall(require, "runic_highlight")
if not ok then
  vim.schedule(function()
    vim.notify(
      string.format("runic_highlight: %s", runic),
      vim.log.levels.ERROR,
      { title = "runic_highlight" }
    )
  end)
  return
end

runic.setup()
