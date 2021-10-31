vim.cmd('highlight NvimTreeCursorLine guibg=#8094B4 guifg=#223E69')
local g = vim.g

g.nvim_tree_gitignore = 1 -- Enable gitignore.
g.nvim_tree_quit_on_open = 0 -- closes tree when file's opened.
g.nvim_tree_indent_markers = 1 -- This option shows indent markers when folders are open.
g.nvim_tree_git_hl = 1 -- Will enable file highlight for git attributes (can be used without the icons).
g.nvim_tree_highlight_opened_files = 0 -- Will enable folder and file icon highlight for opened files/directories.
g.nvim_tree_add_trailing = 0 -- Append a trailing slash to folder names. ]]



require'nvim-tree'.setup {
  update_focused_file = {
    enable      = true,
    update_cwd  = false,
    ignore_list = {".git", "node_modules", ".cache", "__pycache__"}
  },
	view = {
		width = 30,
	},
  diagnostics = {
    enable = true,
    icons = {
      hint = "",
      info = "",
      warning = "",
      error = "",
    }
  },
}
