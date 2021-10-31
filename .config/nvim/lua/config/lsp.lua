local border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" }

local nvim_lsp = require "lspconfig"
local on_attach = function(client, bufnr)
	local buf_map = vim.api.nvim_buf_set_keymap
	buf_map(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", {})
	buf_map(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", {})
	buf_map(bufnr, "n", "E", "<cmd>lua vim.lsp.buf.hover()<CR>", {})
	buf_map(bufnr, "n", "<C-e>", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({border="..vim.inspect(border)..", focusable=true})<CR>", {})
	buf_map(bufnr, "n", "gn", "<cmd>lua vim.lsp.diagnostic.goto_next({ popup_opts = { border = "..vim.inspect(border).." }})<CR>", {})
	buf_map(bufnr, "n", "gp", "<cmd>lua vim.lsp.diagnostic.goto_prev({ popup_opts = { border = "..vim.inspect(border).." }})<CR>", {})
	buf_map(bufnr, "n", "ga", "<cmd>lua vim.lsp.buf.code_action()<CR>", {})

end

vim.lsp.handlers["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, {border = border})
vim.lsp.handlers["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, {border = border})
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


local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

nvim_lsp.gdscript.setup {
	on_attach = on_attach,
	capabilities = capabilities
}

nvim_lsp.graphql.setup{
	on_attach = on_attach,
	capabilities = capabilities
}

nvim_lsp.jsonls.setup{
	on_attach = on_attach,
	capabilities = capabilities
}

local lsp_installer = require("nvim-lsp-installer")
lsp_installer.on_server_ready(function(server)
	local opts = {
		on_attach = on_attach,
		capabilities = capabilities		}

		if server.name == 'omnisharp' then
			opts.cmd = { "/home/amnesia/.local/share/nvim/lsp_servers/omnisharp/omnisharp/run", "--languageserver" , "--hostPID", tostring(vim.fn.getpid()) };
			opts.root_dir = nvim_lsp.util.root_pattern("*.csproj","*.sln");
		elseif server.name == 'hls' then
			opts.cmd =  { "haskell-language-server-wrapper", "--lsp" }
		end
	server:setup(opts)
	vim.cmd [[ do User LspAttachBuffers ]]
end)
