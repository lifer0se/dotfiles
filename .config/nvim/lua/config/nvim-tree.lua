vim.cmd('highlight NvimTreeCursorLine guibg=#8094B4 guifg=#223E69')

vim.g.nvim_tree_indent_markers = 1 -- This option shows indent markers when folders are open.
require'nvim-tree'.setup {
  update_focused_file = {
    enable      = true,
    update_cwd  = false,
  },
	view = {
		width = 30,
	},
}
