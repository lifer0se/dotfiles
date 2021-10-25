vim.highlight.create('CmpItemAbbrMatch', {ctermfg=109, guifg='#87afaf' }, false)
vim.highlight.create('CmpItemAbbrMatchFuzzy', {ctermfg=109, guifg='#87afaf' }, false)
vim.highlight.create('CmpItemKind', {ctermfg=145, guifg='#afafaf' }, false)

local cmp = require "cmp"
cmp.setup({
	mapping = {
		['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
		['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 's' }),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-e>'] = cmp.mapping.close(),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
	},
	formatting = {
		format = require('lspkind').cmp_format({
			with_text = false,
			maxwidth = 50,
		})
	},
	sources = {
		{ name = 'nvim_lsp' },
		{ name = 'nvim_lua' },
		{ name = 'path' },
		{ name = 'buffer' },
	},
})
