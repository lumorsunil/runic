lua << EOF
local buffers = {
    debug_output = nil,
    output = nil,
}

function kill()
    if (buffers.debug_output) then
        vim.cmd("bd! " .. buffers.debug_output)
        buffers.debug_output = nil
    end
    if (buffers.output) then
        vim.cmd("bd! " .. buffers.output)
        buffers.output = nil
    end
end

vim.api.nvim_create_user_command(
    'RunDebugLogging',
    function()
        if vim.fn.expand("%:e") ~= "rn" then return end
        vim.cmd [[rightbelow vnew]]
        buffers.debug_output = vim.fn.bufnr()
        vim.bo.buftype = "nofile"
        vim.bo.filetype = "c"
        vim.api.nvim_buf_set_keymap(0, "n", "q", "", { nowait = true, callback = kill })
        vim.api.nvim_buf_set_keymap(0, "n", "<Esc>", "", { nowait = true, callback = kill })
        vim.cmd [[%!runic --verbose # > output.txt]]
        vim.bo.modifiable = false
        vim.cmd [[belowright new]]
        buffers.output = vim.fn.bufnr()
        vim.bo.buftype = "nofile"
        vim.cmd [[term tail -f output.txt]]
        vim.api.nvim_buf_set_keymap(buffers.output, "n", "q", "", { nowait = true, callback = kill })
        vim.api.nvim_buf_set_keymap(buffers.output, "n", "<Esc>", "", { nowait = true, callback = kill })
        vim.api.nvim_buf_set_keymap(buffers.output, "t", "q", "", { nowait = true, callback = kill })
        vim.api.nvim_buf_set_keymap(buffers.output, "t", "<Esc>", "", { nowait = true, callback = kill })
        vim.bo.modifiable = false
    end,
    {}
)
vim.api.nvim_create_user_command(
    'Run',
    function()
        if vim.fn.expand("%:e") ~= "rn" then return end
        vim.cmd [[rightbelow vnew]]
        vim.bo.buftype = "nofile"
        vim.cmd [[nnoremap <buffer> q <Cmd>bd!<CR>]]
        vim.cmd [[nnoremap <buffer> <Esc> <Cmd>bd!<CR>]]
        vim.cmd [[%!runic #]]
        vim.bo.modifiable = false
    end,
    {}
)
vim.api.nvim_create_user_command(
    'RunDebug',
    function()
        if vim.fn.expand("%:e") ~= "rn" then return end
        vim.cmd [[tabnew]]
        vim.bo.buftype = "nofile"
        vim.cmd [[term runic --debug-ir #]]
    end,
    {}
)
vim.api.nvim_create_user_command(
    'RunDryVerbose',
    function()
        if vim.fn.expand("%:e") ~= "rn" then return end
        vim.cmd [[vnew]]
        vim.cmd [[%!runic --dry-run --verbose #]]
        vim.cmd [[set filetype=runic | set buftype=nofile]]
        vim.cmd [[nnoremap q :bd!<CR>]]
    end,
    {}
)
EOF

nmap <F5> <Cmd>Run<CR>
nmap <F6> <Cmd>RunDebugLogging<CR>
nmap <F7> <Cmd>RunDebug<CR>
nmap <F4> <Cmd>RunDryVerbose<CR>
