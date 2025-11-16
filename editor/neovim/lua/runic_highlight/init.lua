local defaults = {
  minimum_version = { major = 0, minor = 9, patch = 5 },
  filetype = {
    extensions = { rn = "runic" },
    filenames = { Runicfile = "runic" },
    patterns = {},
  },
  buffer = {
    commentstring = "// %s",
    shiftwidth = 2,
    tabstop = 2,
    softtabstop = 2,
    expandtab = true,
  },
}

local config = vim.deepcopy(defaults)

local M = {}

local function version_ok()
  local ver = vim.version()
  if not ver or not ver.major then
    return true
  end
  if ver.major > config.minimum_version.major then
    return true
  end
  if ver.major < config.minimum_version.major then
    return false
  end
  if ver.minor > config.minimum_version.minor then
    return true
  end
  if ver.minor < config.minimum_version.minor then
    return false
  end
  return (ver.patch or 0) >= config.minimum_version.patch
end

local function notify_once(msg, level)
  if M._notified then
    return
  end
  M._notified = true
  vim.schedule(function()
    vim.notify(msg, level, { title = "runic_highlight" })
  end)
end

local function register_filetypes()
  if not (vim.filetype and vim.filetype.add) then
    return
  end
  local spec = {}
  if config.filetype.extensions and next(config.filetype.extensions) then
    spec.extension = config.filetype.extensions
  end
  if config.filetype.filenames and next(config.filetype.filenames) then
    spec.filename = config.filetype.filenames
  end
  if config.filetype.patterns and next(config.filetype.patterns) then
    spec.pattern = config.filetype.patterns
  end
  if next(spec) then
    vim.filetype.add(spec)
  end
end

local function add_undo(buffer_vars, cmd)
  if buffer_vars.undo_ftplugin and buffer_vars.undo_ftplugin ~= "" then
    buffer_vars.undo_ftplugin = buffer_vars.undo_ftplugin .. " | " .. cmd
  else
    buffer_vars.undo_ftplugin = cmd
  end
end

function M.setup(opts)
  if M._setup_done then
    if opts then
      config = vim.tbl_deep_extend("force", config, opts)
    end
    return
  end

  config = vim.tbl_deep_extend("force", vim.deepcopy(defaults), opts or {})
  register_filetypes()
  if not version_ok() then
    notify_once(
      ("Neovim %d.%d.%d+ is required; found %s"):format(
        config.minimum_version.major,
        config.minimum_version.minor,
        config.minimum_version.patch,
        vim.version and vim.inspect(vim.version()) or "unknown"
      ),
      vim.log.levels.WARN
    )
  end
  M._setup_done = true
end

function M.apply_buffer_settings(bufnr)
  bufnr = bufnr or 0
  vim.api.nvim_buf_call(bufnr, function()
    local opts = config.buffer
    local function set_option(name, value)
      if value == nil then
        return
      end
      vim.bo[name] = value
      add_undo(vim.b, ("setlocal %s<"):format(name))
    end

    set_option("commentstring", opts.commentstring)
    set_option("shiftwidth", opts.shiftwidth)
    set_option("tabstop", opts.tabstop)
    set_option("softtabstop", opts.softtabstop)
    set_option("expandtab", opts.expandtab)
  end)
end

function M.config()
  return config
end

return M
