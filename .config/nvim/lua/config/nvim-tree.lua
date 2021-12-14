
vim.g.nvim_tree_indent_markers = 1
require'nvim-tree'.setup {
  hijack_cursor = true,
  update_focused_file = {
    enable = true,
    update_cwd = false,
  },
	view = {
		width = 30,
	},
}
