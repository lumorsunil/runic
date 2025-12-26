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
    'RunDebug',
    function()
        if vim.fn.expand("%:e") ~= "rn" then return end
        vim.cmd [[rightbelow vnew]]
        buffers.debug_output = vim.fn.bufnr()
        vim.bo.buftype = "nofile"
        vim.bo.filetype = "c"
        vim.api.nvim_buf_set_keymap(0, "n", "q", "", { nowait = true, callback = kill })
        vim.api.nvim_buf_set_keymap(0, "n", "<Esc>", "", { nowait = true, callback = kill })
        vim.cmd [[%!runic --enable-ir --verbose # > output.txt]]
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
        vim.cmd [[%!runic --enable-ir #]]
        vim.bo.modifiable = false
    end,
    {}
)
EOF

nmap <F5> <Cmd>RunDebug<CR>
nmap <F6> <Cmd>Run<CR>
