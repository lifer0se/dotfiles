
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
  -- spacing = 0,
  underline = false,
  virtual_text = {
    prefix = ''
  }
})

local signs = { Error = "", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, {  texthl = hl, text = icon, numhl = hl })
end

require "lsp_signature".setup({
  bind = true,
  doc_lines = 0,
  hint_enable = false,
  handler_opts = {
    border = "single"
  },
})

local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
local lsp_installer = require("nvim-lsp-installer")
lsp_installer.on_server_ready(function(server)
  local opts = {
    on_attach = on_attach,
    capabilities = capabilities
    }

    if server.name == 'sumneko_lua' then
      opts.settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' }
          }
        }
      }
    end

  server:setup(opts)
  vim.cmd [[ do User LspAttachBuffers ]]
end)
