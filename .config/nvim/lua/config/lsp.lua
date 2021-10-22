local nvim_lsp = require "lspconfig"

local on_attach = function(client, bufnr)
	local buf_map = vim.api.nvim_buf_set_keymap
	buf_map(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", {})
	buf_map(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.implementation()<CR>", {})
	buf_map(bufnr, "n", "E", "<cmd>lua vim.lsp.buf.hover()<CR>", {})
	buf_map(bufnr, "n", "<C-e>", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({focusable=false})<CR>", {})
	buf_map(bufnr, "n", "ge", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", {})
	buf_map(bufnr, "n", "gE", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", {})
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
	update_in_insert = false,
	spacing = 0,
	underline = false,
	virtual_text = {
		prefix = '●'
	}
})

local signs = { Error = "ﰲ ", Warn = "ﰲ ", Hint = "ﰲ ", Info = "ﰲ " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

require "lsp_signature".setup({
	bind = false,
	doc_lines = 0,
	hint_enable = false,
	handler_opts = {
		border = "none"
	},
})

nvim_lsp.gdscript.setup {
	on_attach = on_attach,
	capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
}

local lsp_installer = require("nvim-lsp-installer")
lsp_installer.on_server_ready(function(server)
	local opts = {
		on_attach = on_attach,
		capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
		}
		if server.name == 'omnisharp' then
			opts.cmd = { "/home/amnesia/.local/share/nvim/lsp_servers/omnisharp/omnisharp/run", "--languageserver" , "--hostPID", tostring(vim.fn.getpid()) };
			opts.root_dir = nvim_lsp.util.root_pattern("*.csproj","*.sln");
		elseif server.name == 'hls' then
			opts.cmd =  { "haskell-language-server-wrapper", "--lsp" }
			-- opts.cwd = "/home/amnesia/.config/xmonad/xmonad"
			-- opts.root_dir = nvim_lsp.util.root_pattern("hie.yaml")
		end
	server:setup(opts)
	vim.cmd [[ do User LspAttachBuffers ]]
end)
