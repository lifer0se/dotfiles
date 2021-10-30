require'nvim-tree'.setup {
  update_focused_file = {
    enable      = true,
    update_cwd  = false,
    ignore_list = {}
  },
	view = {
		width = 40,
	}
}

vim.g.nvim_tree_quit_on_open = 1
vim.cmd('highlight NvimTreeCursorLine guibg=#8094B4 guifg=#223E69')
