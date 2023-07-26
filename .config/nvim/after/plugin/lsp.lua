local lsp = require('lsp-zero')

lsp.preset('recommended')

-- Fix Undefined global 'vim'
lsp.configure('lua_ls', {
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
})


lsp.set_preferences({
    set_lsp_keymaps = false,
    sign_icons = {
        error = '',
        warn = '',
        hint = '',
        info = ''
    }
})

local function on_attach(client, bufnr)

    local opts = {buffer = bufnr, remap = false}

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "E", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<C-e>", vim.diagnostic.open_float, opts)
    vim.keymap.set("n", "gn", vim.diagnostic.goto_next, opts)
    vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)

    -- vim.cmd [[
    --   hi! LspReferenceRead guibg='#202230'
    --   hi! LspReferenceText guibg='#202230'
    --   hi! LspReferenceWrite guibg='#202230'
    -- ]]
    -- vim.api.nvim_create_augroup('lsp_document_highlight', {})
    -- vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
    --   group = 'lsp_document_highlight',
    --   buffer = 0,
    --   callback = vim.lsp.buf.document_highlight,
    -- })
    -- vim.api.nvim_create_autocmd('CursorMoved', {
    --   group = 'lsp_document_highlight',
    --   buffer = 0,
    --   callback = vim.lsp.buf.clear_references,
    -- })

    require "lsp_signature".on_attach({ hint_prefix = " "}, bufnr)

end

lsp.on_attach(on_attach)

lsp.configure('gdscript', {
    force_setup = true,
    on_attach = on_attach
})

lsp.configure('pylsp', {
    on_attach = on_attach,
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
})

lsp.setup()

local cmp = require('cmp')
local cmp_action = require('lsp-zero').cmp_action()
cmp.setup({
    preselect = cmp.PreselectMode.None,
    mapping = {
        -- `Enter` key to confirm completion
        ['<CR>'] = cmp.mapping.confirm({behavior = cmp.ConfirmBehavior.Insert, select = true}),

        ['<Tab>'] = cmp_action.luasnip_supertab(),
        ['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),

        -- Navigate between snippet placeholder
        ['<C-f>'] = cmp_action.luasnip_jump_forward(),
        ['<C-b>'] = cmp_action.luasnip_jump_backward(),
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

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
    sources = {
        { name = 'buffer' }
    }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})

-- If you want insert `(` after select function or method item
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on(
  'confirm_done',
  cmp_autopairs.on_confirm_done()
)
