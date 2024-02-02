-- vim.api.nvim_set_hl(0, "IndentBlanklineChar", { fg = '#3C4050' })
-- vim.api.nvim_set_hl(0, "IndentBlanklineContextChar", { fg = '#51566B' })
--
local highlight = {
    "IblFg",
}
local hooks = require "ibl.hooks"
-- create the highlight groups in the highlight setup hook, so they are reset
-- every time the colorscheme changes
hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
	vim.api.nvim_set_hl(0, "IblFg", { fg = "#51566B" })

end)

require("ibl").setup {
	indent = {
		char = "┃",
		tab_char = "┃",
	},
	scope = {
		highlight = highlight,
		show_start = false,
		show_end = false,
		char = "┃",
	},
}

hooks.register(hooks.type.SCOPE_HIGHLIGHT, hooks.builtin.scope_highlight_from_extmark)
