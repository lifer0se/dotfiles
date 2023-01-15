vim.opt.runtimepath:append("~/Development/ezbookmarks.nvim")
require('ezbookmarks').setup{
    cwd_on_open = 1,
    use_bookmark_dir = 1,
    open_new_tab = 0,
}

vim.keymap.set('n', '<leader>fn', '<cmd>lua require("ezbookmarks").AddBookmark()<CR>')
vim.keymap.set('n', '<leader>fd', '<cmd>lua require("ezbookmarks").AddBookmarkDirectory()<CR>')
vim.keymap.set('n', '<leader>fe', '<cmd>lua require("ezbookmarks").OpenBookmark()<CR>')
vim.keymap.set('n', '<leader>fr', '<cmd>lua require("ezbookmarks").RemoveBookmark()<CR>')
