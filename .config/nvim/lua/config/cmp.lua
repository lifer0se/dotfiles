local cmp = require("cmp")

vim.cmd[["let g:vsnip_filetypes.cs = ['c#']"]]
vim.cmd([[imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>']])
vim.cmd([[smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>']])
vim.cmd([[imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>']])
vim.cmd([[smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>']])
vim.cmd([[imap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>']])
vim.cmd([[smap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>']])

cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  preselect = cmp.PreselectMode.None,

  mapping = {
    ['<C-Space>'] = cmp.mapping.complete({ select = true }),
    ['<C-e>'] = cmp.mapping.close(),
    ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 's' }),
    ['<CR>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),

    ['<Space>'] = function(fallback)
      cmp.mapping.confirm()
      fallback()
    end,
    ['('] = function(fallback)
      cmp.mapping.confirm()
      fallback()
    end,
    ['.'] = function(fallback)
      cmp.mapping.confirm()
      fallback()
    end,
    ['<'] = function(fallback)
      cmp.mapping.confirm()
      fallback()
    end,
    ['['] = function(fallback)
      cmp.mapping.confirm()
      fallback()
    end,
    ['{'] = function(fallback)
      cmp.mapping.confirm()
      fallback()
    end,
  },

  formatting = {
    format = require("lspkind").cmp_format({
      with_text = true,
      menu = ({
        buffer = "",
        nvim_lsp = "曆",
        vsnip = "",
        nvim_lua = "",
      })
    }),
  },

  documentation = {
    border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
  },

  experimental = {
    ghost_text = true,
  },

  sources = cmp.config.sources{
    { name = 'vsnip', keyword_length = 2 },
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'path' },
    { name = 'buffer'},
  }
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
