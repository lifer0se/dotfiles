require'nvim-treesitter.configs'.setup {
	refactor = {
		highlight_current_scope = { enable = false },
		highlight_definitions = {
			enable = true,
			clear_on_cursor_move = true,
		},
		navigation = {
			enable = true,
			keymaps = {
				goto_next_usage = "<C-n>",
				goto_previous_usage = "<C-p>",
			},
		},
	},
}
