require("mason").setup()
require("mason-lspconfig").setup {
	ensure_installed = { "lua_ls" }
}

require "lsp_signature".setup()

vim.diagnostic.config({
	signs = true,
	underline = false,
	update_in_insert = false,
	severity_sort = true,
	virtual_text = {
		prefix = '●'
	}
})

for _, diag in ipairs({ "Error", "Warn", "Info", "Hint" }) do
    vim.fn.sign_define("DiagnosticSign" .. diag, {
        text = "",
        texthl = "DiagnosticSign" .. diag,
        linehl = "",
        numhl = "DiagnosticSign" .. diag,
    })
end

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or "rounded"
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end


vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(args)

		require "lsp_signature".on_attach({
			bind = true,
			hint_prefix = "● ",
			handler_opts = {
				border = "rounded"
			}
		}, args.buf)

		local opts = { buffer = args.buf }
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
		vim.keymap.set("n", "E", vim.lsp.buf.hover, opts)
		vim.keymap.set("n", "<C-e>", vim.diagnostic.open_float, opts)
		vim.keymap.set("n", "gn", vim.diagnostic.goto_next, opts)
		vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, opts)
		vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)
		vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
		vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)
		vim.keymap.set("n", '<leader>sc', vim.lsp.buf.rename, opts)
		-- vim.keymap.set('n', '<C-o>', function()
		-- vim.lsp.buf.format { async = true }
		-- end, opts)
	end,
})


local lspconfig = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').default_capabilities()

lspconfig.gdscript.setup {
    capabilities = capabilities,
}

lspconfig.lua_ls.setup {
    capabilities = capabilities,
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
}
lspconfig.pylsp.setup {
    capabilities = capabilities,
    settings = {
        configurationSources = {"flake8"},
        formatCommand = {"black"},
        pylsp = {
            plugins = {
                pycodestyle={
                    enabled=true,
                    ignore={'E501','E201', 'E202', 'E303', 'E722'},
                    maxLineLength=120},
            }
        }
    }
}
