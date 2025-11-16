local function notify(err)
  vim.schedule(function()
    vim.notify(
      string.format("runic ftplugin: %s", err),
      vim.log.levels.ERROR,
      { title = "runic_highlight" }
    )
  end)
end

local ok, runic = pcall(require, "runic_highlight")
if not ok then
  notify(runic)
  return
end

runic.setup()
runic.apply_buffer_settings(0)
