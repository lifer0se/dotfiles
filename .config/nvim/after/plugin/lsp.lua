local lsp = require('lsp-zero')

lsp.preset('recommended')

-- Fix Undefined global 'vim'
lsp.configure('sumneko_lua', {
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
})

local cmp = require('cmp')
lsp.setup_nvim_cmp({

    preselect = cmp.PreselectMode.None,
    mapping = {
        ['<C-e>'] = cmp.mapping.close(),
        ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 's' }),
        ['<CR>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
    },

    experimental = {
        ghost_text = true,
    },

    sources = cmp.config.sources {
        { name = 'luasnip' },
        { name = 'nvim_lsp' },
        { name = 'nvim_lua' },
        { name = 'path' },
        { name = 'buffer' },
    },

    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
})

lsp.set_preferences({
    set_lsp_keymaps = false,
    sign_icons = {
        error = 'ﰲ',
        warn = 'ﰲ',
        hint = 'ﰲ',
        info = 'ﰲ'
    }
})

lsp.on_attach(function(client, bufnr)

  local opts = {buffer = bufnr, remap = false}

  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
  vim.keymap.set("n", "E", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "<C-e>", vim.diagnostic.open_float, opts)
  vim.keymap.set("n", "gn", vim.diagnostic.goto_next, opts)
  vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
  vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)

end)


lsp.setup()
