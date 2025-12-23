lua << EOF
vim.api.nvim_create_user_command(
    'Run',
    function()
        if vim.fn.expand("%:e") ~= "rn" then return end
        vim.cmd [[rightbelow vnew]]
        vim.bo.buftype = "nofile"
        vim.bo.filetype = "bash"
        vim.cmd [[nnoremap <buffer> q <Cmd>bd!<CR>]]
        vim.cmd [[nnoremap <buffer> <Esc> <Cmd>bd!<CR>]]
        vim.cmd [[%!./runic --enable-ir #]]
        vim.bo.modifiable = false
    end,
    {}
)
EOF

nmap <F5> <Cmd>Run<CR>
